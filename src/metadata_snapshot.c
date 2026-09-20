#include <R.h>
#include <Rinternals.h>
#include <Rversion.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include "metadata_text.h"

/* Owned storage: no SEXP or pointer into R memory survives capture. */
typedef struct metadata_node {
    int type;
    R_xlen_t length;
    void *values;
    int *encodings;
    struct metadata_node *names;
} metadata_node;

static void release_node(metadata_node *node) {
    if (node == NULL) return;
    if (node->values != NULL && node->type == VECSXP) {
        metadata_node **children = node->values;
        for (R_xlen_t i = 0; i < node->length; i++) release_node(children[i]);
    }
    if (node->values != NULL && node->type == STRSXP) {
        char **strings = node->values;
        for (R_xlen_t i = 0; i < node->length; i++) free(strings[i]);
    }
    release_node(node->names);
    free(node->encodings);
    free(node->values);
    free(node);
}

static void *allocate_items(R_xlen_t count, size_t width) {
    if ((uint64_t)count > SIZE_MAX / width) Rf_error("Metadata snapshot is too large.");
    void *result = calloc(count ? (size_t)count : 1, width);
    if (result == NULL) Rf_error("Cannot allocate metadata snapshot.");
    return result;
}

static void capture_node(SEXP value, metadata_node **destination, int depth) {
    if (depth > 8) Rf_error("Metadata nesting exceeds snapshot limit.");
    metadata_node *node = allocate_items(1, sizeof(*node));
    *destination = node; /* Publish ownership before any operation can raise. */
    node->type = TYPEOF(value);
    node->length = value == R_NilValue ? 0 : XLENGTH(value);
    size_t width = 0;
    switch (node->type) {
        case NILSXP: return;
        case VECSXP: width = sizeof(metadata_node *); break;
        case STRSXP: width = sizeof(char *); break;
        case INTSXP: case LGLSXP: width = sizeof(int); break;
        case REALSXP: width = sizeof(double); break;
        default: Rf_error("Unsupported metadata storage type.");
    }
    /* Reject rather than silently discard an unsupported attribute. */
#if R_VERSION >= R_Version(4, 6, 0)
    SEXP attribute_names = PROTECT(R_getAttribNames(value));
    for (R_xlen_t i = 0; attribute_names != R_NilValue && i < XLENGTH(attribute_names); i++) {
        if (strcmp(CHAR(STRING_ELT(attribute_names, i)), "names") != 0)
            Rf_error("Unsupported metadata attribute.");
    }
    UNPROTECT(1);
#else
    for (SEXP attribute = ATTRIB(value); attribute != R_NilValue; attribute = CDR(attribute)) {
        if (TAG(attribute) != R_NamesSymbol) Rf_error("Unsupported metadata attribute.");
    }
#endif
    node->values = allocate_items(node->length, width);
    if (node->type == VECSXP) {
        metadata_node **children = node->values;
        for (R_xlen_t i = 0; i < node->length; i++) {
            capture_node(VECTOR_ELT(value, i), &children[i], depth + 1);
        }
    } else if (node->type == STRSXP) {
        char **strings = node->values;
        node->encodings = allocate_items(node->length, sizeof(int));
        for (R_xlen_t i = 0; i < node->length; i++) {
            SEXP string = STRING_ELT(value, i);
            if (string == NA_STRING) continue;
            size_t length = (size_t)LENGTH(string);
            strings[i] = allocate_items(length + 1, 1);
            memcpy(strings[i], CHAR(string), length + 1);
            node->encodings[i] = getCharCE(string);
        }
    } else {
        const void *source = node->type == REALSXP ? (const void *)REAL(value) :
            node->type == INTSXP ? (const void *)INTEGER(value) : (const void *)LOGICAL(value);
        memcpy(node->values, source, (size_t)node->length * width);
    }
    if (getAttrib(value, R_NamesSymbol) != R_NilValue) {
        capture_node(getAttrib(value, R_NamesSymbol), &node->names, depth + 1);
    }
}

/* Capture bare column values. Metadata and S3 behavior are captured separately
 * on the R thread; this buffer is an immutable input for value analysis. */
static void capture_value_node(SEXP value, metadata_node **destination) {
    metadata_node *node = allocate_items(1, sizeof(*node));
    size_t width = 0;

    *destination = node;
    node->type = TYPEOF(value);
    node->length = XLENGTH(value);

    switch (node->type) {
        case STRSXP:
            width = sizeof(char *);
            break;
        case INTSXP:
        case LGLSXP:
            width = sizeof(int);
            break;
        case REALSXP:
            width = sizeof(double);
            break;
        default:
            Rf_error("Unsupported value storage type in metadata session.");
    }

    node->values = allocate_items(node->length, width);

    if (node->type == STRSXP) {
        char **strings = node->values;

        node->encodings = allocate_items(node->length, sizeof(int));

        for (R_xlen_t i = 0; i < node->length; i++) {
            SEXP string = STRING_ELT(value, i);

            if (string == NA_STRING) {
                continue;
            }

            size_t length = (size_t)LENGTH(string);

            strings[i] = allocate_items(length + 1, 1);
            memcpy(strings[i], CHAR(string), length + 1);
            node->encodings[i] = getCharCE(string);
        }
    }
    else {
        const void *source = node->type == REALSXP ? (const void *)REAL(value) :
            node->type == INTSXP ? (const void *)INTEGER(value) :
                (const void *)LOGICAL(value);

        memcpy(
            node->values,
            source,
            (size_t)node->length * width
        );
    }
}

static SEXP restore_node(const metadata_node *node) {
    if (node->type == NILSXP) return R_NilValue;
    SEXP value = PROTECT(allocVector(node->type, node->length));
    if (node->type == VECSXP) {
        metadata_node **children = node->values;
        for (R_xlen_t i = 0; i < node->length; i++) SET_VECTOR_ELT(value, i, restore_node(children[i]));
    } else if (node->type == STRSXP) {
        char **strings = node->values;
        for (R_xlen_t i = 0; i < node->length; i++) {
            SET_STRING_ELT(value, i, strings[i] == NULL ? NA_STRING :
                mkCharCE(strings[i], (cetype_t)node->encodings[i]));
        }
    } else {
        void *target = node->type == REALSXP ? (void *)REAL(value) :
            node->type == INTSXP ? (void *)INTEGER(value) : (void *)LOGICAL(value);
        memcpy(target, node->values, (size_t)node->length *
            (node->type == REALSXP ? sizeof(double) : sizeof(int)));
    }
    if (node->names != NULL) {
        SEXP names = PROTECT(restore_node(node->names));
        setAttrib(value, R_NamesSymbol, names);
        UNPROTECT(1);
    }
    UNPROTECT(1);
    return value;
}

static SEXP snapshot_tag(void) { return install("DDIwR.metadata.snapshot.v1"); }

static void finalize_snapshot(SEXP pointer) {
    release_node(R_ExternalPtrAddr(pointer));
    R_ClearExternalPtr(pointer);
}

typedef struct { SEXP input; SEXP pointer; metadata_node *root; SEXP fields; } capture_context;
static SEXP capture_body(void *context) {
    capture_context *capture = context;
    capture_node(capture->input, &capture->root, 0);
    R_SetExternalPtrAddr(capture->pointer, capture->root);
    capture->root = NULL;
    return R_NilValue;
}
static void capture_cleanup(void *context) {
    capture_context *capture = context;
    release_node(capture->root);
}

static SEXP clean_text_body(void *context) {
    capture_context *capture = context;
    capture_node(capture->input, &capture->root, 0);
    metadata_node *node = capture->root;
    SEXP fallback = PROTECT(allocVector(LGLSXP, node->length));
    char **strings = node->values;
    for (R_xlen_t i = 0; i < node->length; i++) {
        LOGICAL(fallback)[i] = 0;
        if (strings[i] == NULL) continue;
        char *cleaned = NULL;
        int status = ddiwr_metadata_clean_text(strings[i], &cleaned);
        if (status < 0) Rf_error("Cannot allocate normalized metadata text.");
        if (status == 1) {
            LOGICAL(fallback)[i] = 1;
        } else {
            free(strings[i]);
            strings[i] = cleaned;
        }
    }
    SEXP result = PROTECT(allocVector(VECSXP, 2));
    SET_VECTOR_ELT(result, 0, restore_node(node));
    SET_VECTOR_ELT(result, 1, fallback);
    UNPROTECT(2);
    return result;
}

SEXP metadata_clean_text(SEXP text) {
    if (TYPEOF(text) != STRSXP) Rf_error("Metadata text must be character.");
    capture_context capture = {text, R_NilValue, NULL, R_NilValue};
    return R_ExecWithCleanup(clean_text_body, &capture, capture_cleanup, &capture);
}

SEXP metadata_snapshot_create(SEXP records) {
    if (TYPEOF(records) != VECSXP) Rf_error("Metadata records must be a list.");
    SEXP pointer = PROTECT(R_MakeExternalPtr(NULL, snapshot_tag(), R_NilValue));
    R_RegisterCFinalizerEx(pointer, finalize_snapshot, TRUE);
    capture_context capture = {records, pointer, NULL, R_NilValue};
    R_ExecWithCleanup(capture_body, &capture, capture_cleanup, &capture);
    UNPROTECT(1);
    return pointer;
}

static SEXP capture_raw_body(void *context) {
    capture_context *capture = context;
    metadata_node *root = allocate_items(1, sizeof(*root));
    capture->root = root;
    root->type = VECSXP;
    root->length = XLENGTH(capture->input);
    root->values = allocate_items(root->length, sizeof(metadata_node *));
    SEXP column_names = getAttrib(capture->input, R_NamesSymbol);
    if (column_names != R_NilValue) capture_node(column_names, &root->names, 0);
    metadata_node **columns = root->values;
    for (R_xlen_t i = 0; i < root->length; i++) {
        if (i % 256 == 0) R_CheckUserInterrupt();
        SEXP column = VECTOR_ELT(capture->input, i);
        SEXP record = PROTECT(allocVector(VECSXP, XLENGTH(capture->fields)));
        for (R_xlen_t j = 0; j < XLENGTH(capture->fields); j++) {
            const char *field = CHAR(STRING_ELT(capture->fields, j));
            SEXP value;
            if (strcmp(field, "storage") == 0) {
                value = PROTECT(mkString(type2char(TYPEOF(column))));
            } else if (strcmp(field, "length") == 0) {
                value = PROTECT(ScalarReal((double)XLENGTH(column)));
            } else if (strcmp(field, "position") == 0) {
                value = PROTECT(ScalarReal((double)i + 1));
            } else {
                value = PROTECT(getAttrib(column, install(strcmp(field, "classes") == 0 ? "class" : field)));
            }
            SET_VECTOR_ELT(record, j, value);
            UNPROTECT(1);
        }
        setAttrib(record, R_NamesSymbol, capture->fields);
        capture_node(record, &columns[i], 0);
        UNPROTECT(1);
    }
    R_SetExternalPtrAddr(capture->pointer, root);
    capture->root = NULL;
    return R_NilValue;
}

SEXP metadata_snapshot_raw(SEXP data, SEXP fields) {
    if (TYPEOF(data) != VECSXP || TYPEOF(fields) != STRSXP)
        Rf_error("Raw metadata capture requires a list and character fields.");
    for (R_xlen_t i = 0; i < XLENGTH(fields); i++) {
        if (STRING_ELT(fields, i) == NA_STRING) Rf_error("Missing metadata field name.");
    }
    SEXP pointer = PROTECT(R_MakeExternalPtr(NULL, snapshot_tag(), R_NilValue));
    R_RegisterCFinalizerEx(pointer, finalize_snapshot, TRUE);
    capture_context capture = {data, pointer, NULL, fields};
    R_ExecWithCleanup(capture_raw_body, &capture, capture_cleanup, &capture);
    UNPROTECT(1);
    return pointer;
}

typedef struct {
    SEXP data;
    SEXP columns;
    metadata_node *root;
} value_copy_context;

static SEXP copy_values_body(void *context) {
    value_copy_context *copy = context;
    metadata_node *root = allocate_items(1, sizeof(*root));
    metadata_node **values = NULL;
    SEXP source_names = getAttrib(copy->data, R_NamesSymbol);

    copy->root = root;
    root->type = VECSXP;
    root->length = XLENGTH(copy->columns);
    root->values = allocate_items(root->length, sizeof(metadata_node *));
    values = root->values;

    if (source_names != R_NilValue) {
        SEXP selected_names = PROTECT(allocVector(STRSXP, root->length));

        for (R_xlen_t i = 0; i < root->length; i++) {
            int position = INTEGER(copy->columns)[i] - 1;

            SET_STRING_ELT(selected_names, i, STRING_ELT(source_names, position));
        }

        capture_node(selected_names, &root->names, 0);
        UNPROTECT(1);
    }

    for (R_xlen_t i = 0; i < root->length; i++) {
        int position = INTEGER(copy->columns)[i] - 1;

        if (i % 64 == 0) {
            R_CheckUserInterrupt();
        }

        capture_value_node(VECTOR_ELT(copy->data, position), &values[i]);
    }

    SEXP result = PROTECT(restore_node(root));

    release_node(root);
    copy->root = NULL;
    UNPROTECT(1);

    return result;
}

static void copy_values_cleanup(void *context) {
    value_copy_context *copy = context;

    release_node(copy->root);
}

SEXP metadata_values_copy(SEXP data, SEXP columns) {
    if (TYPEOF(data) != VECSXP || TYPEOF(columns) != INTSXP) {
        Rf_error("Value capture requires a data frame and integer positions.");
    }

    for (R_xlen_t i = 0; i < XLENGTH(columns); i++) {
        int position = INTEGER(columns)[i];

        if (position < 1 || position > XLENGTH(data)) {
            Rf_error("Column position is outside the value source.");
        }
    }

    value_copy_context copy = {data, columns, NULL};

    return R_ExecWithCleanup(
        copy_values_body,
        &copy,
        copy_values_cleanup,
        &copy
    );
}

static SEXP restore_fields(const metadata_node *record, SEXP fields) {
    if (fields == R_NilValue) return restore_node(record);
    if (record->type != VECSXP || record->names == NULL)
        Rf_error("Snapshot record has no named fields.");
    char **available = record->names->values;
    R_xlen_t count = 0;
    for (R_xlen_t i = 0; i < XLENGTH(fields); i++) {
        for (R_xlen_t j = 0; j < record->length; j++) {
            if (available[j] != NULL && strcmp(CHAR(STRING_ELT(fields, i)), available[j]) == 0) count++;
        }
    }
    SEXP result = PROTECT(allocVector(VECSXP, count));
    SEXP names = PROTECT(allocVector(STRSXP, count));
    metadata_node **children = record->values;
    R_xlen_t index = 0;
    for (R_xlen_t i = 0; i < XLENGTH(fields); i++) {
        for (R_xlen_t j = 0; j < record->length; j++) {
            if (available[j] != NULL && strcmp(CHAR(STRING_ELT(fields, i)), available[j]) == 0) {
                SET_VECTOR_ELT(result, index, restore_node(children[j]));
                SET_STRING_ELT(names, index++, STRING_ELT(fields, i));
            }
        }
    }
    setAttrib(result, R_NamesSymbol, names);
    UNPROTECT(2);
    return result;
}

SEXP metadata_snapshot_read(SEXP pointer, SEXP columns, SEXP fields) {
    if (TYPEOF(pointer) != EXTPTRSXP || R_ExternalPtrTag(pointer) != snapshot_tag() ||
        R_ExternalPtrAddr(pointer) == NULL) Rf_error("Metadata snapshot is closed or invalid.");
    const metadata_node *root = R_ExternalPtrAddr(pointer);
    if (TYPEOF(columns) != INTSXP) Rf_error("Columns must be integer positions.");
    if (fields != R_NilValue && TYPEOF(fields) != STRSXP) Rf_error("Fields must be character names.");
    for (R_xlen_t i = 0; i < XLENGTH(columns); i++) {
        if (INTEGER(columns)[i] < 1 || INTEGER(columns)[i] > root->length)
            Rf_error("Column position is outside the snapshot.");
    }
    SEXP result = PROTECT(allocVector(VECSXP, XLENGTH(columns)));
    SEXP names = PROTECT(allocVector(STRSXP, XLENGTH(columns)));
    metadata_node **children = root->values;
    for (R_xlen_t i = 0; i < XLENGTH(columns); i++) {
        int position = INTEGER(columns)[i] - 1;
        SET_VECTOR_ELT(result, i, restore_fields(children[position], fields));
        if (root->names != NULL) {
            char **strings = root->names->values;
            SET_STRING_ELT(names, i, strings[position] == NULL ? NA_STRING :
                mkCharCE(strings[position], (cetype_t)root->names->encodings[position]));
        }
    }
    if (root->names != NULL) setAttrib(result, R_NamesSymbol, names);
    UNPROTECT(2);
    return result;
}

SEXP metadata_snapshot_close(SEXP pointer) {
    if (TYPEOF(pointer) != EXTPTRSXP || R_ExternalPtrTag(pointer) != snapshot_tag())
        Rf_error("Invalid metadata snapshot.");
    finalize_snapshot(pointer);
    return R_NilValue;
}

typedef struct {
    SEXP pointer, columns, fields;
    int threads;
    ddiwr_text_job *jobs;
    unsigned long count;
} normalize_context;

static void normalize_cleanup(void *context) {
    normalize_context *work = context;
    if (work->jobs == NULL) return;
    for (unsigned long i = 0; i < work->count; i++) {
        ddiwr_text_job *job = &work->jobs[i];
        if (job->output != NULL) {
            for (unsigned long j = 0; j < job->count; j++) free(job->output[j]);
        }
        free(job->input);
        free(job->output);
    }
    free(work->jobs);
}

static int text_field(const char *name) {
    return strcmp(name, "label") == 0 || strcmp(name, "measurement") == 0 ||
        strcmp(name, "labels") == 0;
}

static SEXP normalize_body(void *context) {
    normalize_context *work = context;
    SEXP records = PROTECT(metadata_snapshot_read(work->pointer, work->columns, work->fields));
    const metadata_node *root = R_ExternalPtrAddr(work->pointer);
    metadata_node **source_columns = root->values;
    for (R_xlen_t i = 0; i < XLENGTH(records); i++) work->count += XLENGTH(VECTOR_ELT(records, i));
    work->jobs = allocate_items(work->count, sizeof(ddiwr_text_job));
    unsigned long index = 0;
    for (R_xlen_t i = 0; i < XLENGTH(records); i++) {
        SEXP record = VECTOR_ELT(records, i);
        SEXP names = getAttrib(record, R_NamesSymbol);
        const metadata_node *source = source_columns[INTEGER(work->columns)[i] - 1];
        char **source_names = source->names == NULL ? NULL : source->names->values;
        metadata_node **values = source->values;
        for (R_xlen_t j = 0; j < XLENGTH(record); j++, index++) {
            const char *field = CHAR(STRING_ELT(names, j));
            if (!text_field(field)) continue;
            const metadata_node *value = NULL;
            for (R_xlen_t k = 0; k < source->length; k++) {
                if (source_names && source_names[k] && strcmp(source_names[k], field) == 0) value = values[k];
            }
            if (value == NULL) continue;
            ddiwr_text_job *job = &work->jobs[index];
            R_xlen_t nvalues = value->type == STRSXP ? value->length : 0;
            R_xlen_t nnames = strcmp(field, "labels") == 0 && value->names ? value->names->length : 0;
            job->count = nvalues + nnames;
            job->input = allocate_items(job->count, sizeof(char *));
            job->output = allocate_items(job->count, sizeof(char *));
            for (R_xlen_t k = 0; k < nvalues; k++) job->input[k] = ((char **)value->values)[k];
            for (R_xlen_t k = 0; k < nnames; k++) job->input[nvalues + k] = ((char **)value->names->values)[k];
        }
    }
    int used = ddiwr_metadata_text_jobs(work->jobs, work->count, work->threads);
    index = 0;
    for (R_xlen_t i = 0; i < XLENGTH(records); i++) {
        SEXP record = VECTOR_ELT(records, i);
        SEXP names = getAttrib(record, R_NamesSymbol);
        SEXP pending = PROTECT(allocVector(LGLSXP, XLENGTH(record)));
        for (R_xlen_t j = 0; j < XLENGTH(record); j++, index++) {
            ddiwr_text_job *job = &work->jobs[index];
            if (job->status < 0) Rf_error("Cannot allocate normalized metadata text.");
            SEXP value = VECTOR_ELT(record, j);
            const char *field = CHAR(STRING_ELT(names, j));
            int fallback = job->status == 1 || (text_field(field) && strcmp(field, "labels") != 0 &&
                value != R_NilValue && TYPEOF(value) != STRSXP);
            LOGICAL(pending)[j] = fallback;
            if (fallback || !text_field(field)) continue;
            R_xlen_t nvalues = TYPEOF(value) == STRSXP ? XLENGTH(value) : 0;
            for (R_xlen_t k = 0; k < nvalues; k++) {
                if (job->output[k]) SET_STRING_ELT(value, k, mkChar(job->output[k]));
            }
            SEXP label_names = strcmp(field, "labels") == 0 ? getAttrib(value, R_NamesSymbol) : R_NilValue;
            if (label_names != R_NilValue) {
                for (R_xlen_t k = 0; k < XLENGTH(label_names); k++) {
                    if (job->output[nvalues + k]) SET_STRING_ELT(label_names, k, mkChar(job->output[nvalues + k]));
                }
            }
        }
        setAttrib(record, install("ddiwr_cleanup_pending"), pending);
        UNPROTECT(1);
    }
    SEXP result = PROTECT(allocVector(VECSXP, 2));
    SET_VECTOR_ELT(result, 0, records);
    SET_VECTOR_ELT(result, 1, ScalarInteger(used));
    UNPROTECT(2);
    return result;
}

SEXP metadata_snapshot_normalized(SEXP pointer, SEXP columns, SEXP fields, SEXP threads) {
    if (TYPEOF(threads) != INTSXP || XLENGTH(threads) != 1 || INTEGER(threads)[0] < 1 || INTEGER(threads)[0] > 4)
        Rf_error("Metadata threads must be between one and four.");
    normalize_context work = {pointer, columns, fields, INTEGER(threads)[0], NULL, 0};
    return R_ExecWithCleanup(normalize_body, &work, normalize_cleanup, &work);
}
