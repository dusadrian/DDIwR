#include <R.h>
#include <Rinternals.h>

/* Reader workers use C allocation without calling into R. */
#define SC_CALLOC(n, type) (type *)calloc((size_t)(n), sizeof(type))
#define SC_REALLOC(p, n, type) (type *)realloc((p), (size_t)(n) * sizeof(type))
#define SC_FREE(p) free(p)

#include <ctype.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifndef _WIN32
#include <pthread.h>
#include <unistd.h>
#endif

#include "readstat.h"
#include "spss_attributes.h"
#include "stata_characteristics.h"
#include "readstat_bits.h"
#include "readstat_iconv.h"
#include "readstat_convert.h"
#include "declared_types.h"
#include "vendor_metadata.h"
#include "tagged_na.h"
#include "spss/readstat_sav.h"
#include "spss/readstat_sav_compress.h"
#include "stata/readstat_dta.h"

extern SEXP declared_df_parse_dta_file(SEXP spec, SEXP encoding, SEXP cols_skip, SEXP n_max, SEXP rows_skip);

SEXP readstat_parallel_supported_(void) {
#if defined(__EMSCRIPTEN__) && !defined(DDIWR_WASM_PTHREADS)
    return Rf_ScalarLogical(0);
#else
    return Rf_ScalarLogical(1);
#endif
}

typedef enum {
    LABELSET_NONE = 0,
    LABELSET_STRING = 1,
    LABELSET_DOUBLE = 2
} SCLabelSetType;

typedef struct {
    char *name;
    SCLabelSetType type;
    size_t n;
    size_t cap;
    char **labels;
    char **svalues;
    double *dvalues;
} SCLabelSet;

typedef struct {
    int source_index;
    int kept_index;
    int readstat_type;
    VarType var_type;
    char *name;
    char *label;
    char *format;
    char *val_labels_name;
    SEXP vendor_metadata;
    int has_display_width;
    int display_width;
    size_t storage_width;
    int num_na_values_n;
    double num_na_values[3];
    int has_num_na_range;
    double num_na_range[2];
    int str_na_values_n;
    char *str_na_values[3];
    int has_str_na_range;
    char *str_na_range[2];
} SCColumnSchema;

typedef struct {
    FileExt ext;
    FileVendor vendor;
    long total_rows;
    int ncols;
    char *file_label;
    SEXP vendor_metadata;
    char **notes;
    size_t notes_n;
    readstat_attribute_t *spss_attributes;
    readstat_attribute_t *stata_characteristics;
    SCColumnSchema *cols;
    SCLabelSet *label_sets;
    size_t label_set_n;
    size_t label_set_cap;
} SCSchema;

typedef struct {
    FileExt ext;
    int metadata_probe;
    int row_count_known;
    int row_offset_limit;
    int efficient_row_chunks;
    int parallel_supported_now;
} SCFormatCaps;

typedef struct {
    FileExt ext;
    int user_na;
    long total_rows;
    long requested_skip;
    long requested_nmax;
    long window_start;
    long window_rows;
    int n_workers;
    SCFormatCaps caps;
} SCImportPlan;

static long sc_min_rows_per_chunk(long total_rows) {
    if (total_rows < 100000) {
        return 10000;
    }
    if (total_rows < 1000000) {
        return 50000;
    }
    return 100000;
}

typedef struct {
    int position;
    char tag;
} SCTaggedRow;

typedef struct {
    double *dvalues;
    char **svalues;
    unsigned char *smissing;
    size_t na_index_n;
    size_t na_index_cap;
    SCTaggedRow *tagged;
} SCChunkColumn;

typedef struct {
    long nrows;
    int ncols;
    SCChunkColumn *cols;
} SCChunkResult;

typedef struct {
    long start_row;
    long row_count;
    SCChunkResult *decoded;
} SCChunk;

typedef struct {
    SCSchema *schema;
    SCChunk *chunks;
    int count;
    int user_na;
    void *workers;
} SCOutput;


typedef struct {
    int mode;
    const char *path;
    const char *encoding;
    const int *cols_skip;
    int cols_skip_n;
    SCSchema *schema;
    SCChunkResult *chunk_result;
} SCParseState;

typedef struct {
    SCImportPlan *plan;
    const SCSchema *schema;
    const char *path;
    const char *encoding;
    const int *cols_skip;
    int cols_skip_n;
    SCChunk chunk;
    const dta_ctx_t *dta_ctx;
    int64_t dta_rows_offset;
    int use_custom_dta;
    SCChunkResult result_data;
    readstat_error_t error;
    char error_message[256];
#ifndef _WIN32
    pthread_t thread;
    int thread_started;
#endif
} SCWorkerState;

typedef struct {
    long row_start;
    long long file_offset;
    unsigned char chunk[8];
    int state_i;
} SCSavCheckpoint;

typedef struct {
    long nrows;
    size_t row_width;
    unsigned char *rows;
} SCSavChunkRows;

typedef struct {
    const char *path;
    ddiwr_sav_row_stream_info_t info;
    SCSavCheckpoint checkpoint;
    long row_count;
    long actual_rows;
    SCSavChunkRows result;
    const SCSchema *schema;
    const sav_ctx_t *sav_ctx;
    const char *input_encoding;
    int user_na;
    SCChunkResult decoded;
#ifndef _WIN32
    pthread_t thread;
    int thread_started;
#endif
    int error;
} SCSavWorkerState;

static void sc_sav_tag_missing_double(readstat_value_t *value, const sav_ctx_t *ctx);

static char *sc_strdup(const char *x) {
    size_t n;
    char *out;
    if (x == NULL) {
        return NULL;
    }
    n = strlen(x);
    out = (char *) SC_CALLOC(n + 1, char);
    if (out != NULL) memcpy(out, x, n + 1);
    return out;
}

static unsigned long long sc_fnv1a64_update(unsigned long long hash, const unsigned char *data, size_t n) {
    size_t i;
    for (i = 0; i < n; ++i) {
        hash ^= (unsigned long long) data[i];
        hash *= 1099511628211ULL;
    }
    return hash;
}

static double sc_tag_code_value(char tag) {
    char lower = (char) tolower((unsigned char) tag);
    if (lower < 'a' || lower > 'z') {
        return NA_REAL;
    }
    return -91.0 - (double) (lower - 'a');
}

static SEXP sc_tag_code_name(char tag) {
    char buffer[32];
    double code = sc_tag_code_value(tag);
    if (ISNAN(code)) {
        return NA_STRING;
    }
    snprintf(buffer, sizeof(buffer), "%.15g", code);
    return Rf_mkCharCE(buffer, CE_UTF8);
}

static int sc_cols_skip_contains(const int *cols_skip, int cols_skip_n, int index) {
    int i;
    for (i = 0; i < cols_skip_n; ++i) {
        if (cols_skip[i] == index) {
            return 1;
        }
    }
    return 0;
}

static int sc_spec_path_cstr(SEXP spec, const char **path) {
    SEXP path_sexp;
    if (!Rf_isNewList(spec) || Rf_length(spec) < 1) {
        return 0;
    }
    path_sexp = VECTOR_ELT(spec, 0);
    if (!Rf_isString(path_sexp) || Rf_length(path_sexp) != 1) {
        return 0;
    }
    *path = CHAR(STRING_ELT(path_sexp, 0));
    return 1;
}

static int sc_labelset_ensure_capacity(SCLabelSet *set) {
    if (set->n < set->cap) return 1;
    if (set->cap > SIZE_MAX / (2 * sizeof(double)) ||
            set->cap > SIZE_MAX / (2 * sizeof(char *))) return 0;
    size_t capacity = set->cap ? set->cap * 2 : 8;
    char **labels = SC_REALLOC(set->labels, capacity, char *);
    if (!labels) return 0;
    set->labels = labels;
    if (set->type == LABELSET_STRING) {
        char **values = SC_REALLOC(set->svalues, capacity, char *);
        if (!values) return 0;
        set->svalues = values;
    } else {
        double *values = SC_REALLOC(set->dvalues, capacity, double);
        if (!values) return 0;
        set->dvalues = values;
    }
    set->cap = capacity;
    return 1;
}

static SCLabelSet *sc_find_or_create_labelset(SCSchema *schema, const char *name, SCLabelSetType type) {
    size_t i;
    SCLabelSet *set;

    for (i = 0; i < schema->label_set_n; ++i) {
        if (strcmp(schema->label_sets[i].name, name) == 0) {
            return &schema->label_sets[i];
        }
    }

    if (schema->label_set_n >= schema->label_set_cap) {
        if (schema->label_set_cap > SIZE_MAX / (2 * sizeof(SCLabelSet))) return NULL;
        size_t capacity = schema->label_set_cap ? schema->label_set_cap * 2 : 8;
        SCLabelSet *sets = SC_REALLOC(schema->label_sets, capacity, SCLabelSet);
        if (!sets) return NULL;
        schema->label_sets = sets;
        schema->label_set_cap = capacity;
    }

    set = &schema->label_sets[schema->label_set_n++];
    memset(set, 0, sizeof(*set));
    set->name = sc_strdup(name);
    set->type = type;
    return set->name ? set : NULL;
}

static SCLabelSet *sc_find_labelset(const SCSchema *schema, const char *name) {
    size_t i;
    if (name == NULL || *name == '\0') {
        return NULL;
    }
    for (i = 0; i < schema->label_set_n; ++i) {
        if (strcmp(schema->label_sets[i].name, name) == 0) {
            return &schema->label_sets[i];
        }
    }
    return NULL;
}

static void sc_free_schema(SCSchema *schema) {
    int i;
    size_t j;
    if (schema == NULL) {
        return;
    }
    if (schema->vendor_metadata != NULL) {
        R_ReleaseObject(schema->vendor_metadata);
    }
    for (j = 0; j < schema->notes_n; j++) {
        SC_FREE(schema->notes[j]);
    }
    if (schema->notes != NULL) {
        SC_FREE(schema->notes);
    }
    readstat_attributes_free(schema->spss_attributes);
    readstat_attributes_free(schema->stata_characteristics);
    if (schema->file_label != NULL) {
        SC_FREE(schema->file_label);
    }
    if (schema->cols != NULL) {
        for (i = 0; i < schema->ncols; ++i) {
            int k;
            if (schema->cols[i].vendor_metadata != NULL) {
                R_ReleaseObject(schema->cols[i].vendor_metadata);
            }
            SC_FREE(schema->cols[i].name);
            SC_FREE(schema->cols[i].label);
            SC_FREE(schema->cols[i].format);
            SC_FREE(schema->cols[i].val_labels_name);
            for (k = 0; k < schema->cols[i].str_na_values_n; ++k) {
                SC_FREE(schema->cols[i].str_na_values[k]);
            }
            if (schema->cols[i].has_str_na_range) {
                SC_FREE(schema->cols[i].str_na_range[0]);
                SC_FREE(schema->cols[i].str_na_range[1]);
            }
        }
        SC_FREE(schema->cols);
    }
    for (j = 0; j < schema->label_set_n; ++j) {
        size_t k;
        SCLabelSet *set = &schema->label_sets[j];
        SC_FREE(set->name);
        for (k = 0; k < set->n; ++k) {
            SC_FREE(set->labels[k]);
            if (set->type == LABELSET_STRING) {
                SC_FREE(set->svalues[k]);
            }
        }
        if (set->labels != NULL) {
            SC_FREE(set->labels);
        }
        if (set->svalues != NULL) {
            SC_FREE(set->svalues);
        }
        if (set->dvalues != NULL) {
            SC_FREE(set->dvalues);
        }
    }
    if (schema->label_sets != NULL) {
        SC_FREE(schema->label_sets);
    }
}

static void sc_free_chunk_result(SCChunkResult *result) {
    int i;
    if (result == NULL || result->cols == NULL) {
        return;
    }
    for (i = 0; i < result->ncols; ++i) {
        size_t j;
        if (result->cols[i].dvalues != NULL) {
            SC_FREE(result->cols[i].dvalues);
        }
        if (result->cols[i].svalues != NULL) {
            for (j = 0; j < (size_t) result->nrows; ++j) {
                if (result->cols[i].svalues[j] != NULL) {
                    SC_FREE(result->cols[i].svalues[j]);
                }
            }
            SC_FREE(result->cols[i].svalues);
        }
        if (result->cols[i].smissing != NULL) {
            SC_FREE(result->cols[i].smissing);
        }
        SC_FREE(result->cols[i].tagged);
    }
    SC_FREE(result->cols);
    result->cols = NULL;
}

static SCFormatCaps sc_get_format_capabilities(FileExt ext) {
    SCFormatCaps caps;
    memset(&caps, 0, sizeof(caps));
    caps.ext = ext;
    caps.metadata_probe = 1;
    caps.row_offset_limit = 1;

    switch (ext) {
        case DECLARED_DTA:
            caps.row_count_known = 1;
            caps.efficient_row_chunks = 1;
            caps.parallel_supported_now = 1;
            break;
        case DECLARED_SAV:
            caps.row_count_known = 1;
            caps.efficient_row_chunks = 0;
            caps.parallel_supported_now = 0;
            break;
        case DECLARED_SAS7BDAT:
            caps.row_count_known = 1;
            caps.efficient_row_chunks = 0;
            caps.parallel_supported_now = 0;
            break;
        case DECLARED_POR:
        case DECLARED_XPT:
        default:
            caps.row_count_known = 0;
            caps.efficient_row_chunks = 0;
            caps.parallel_supported_now = 0;
            break;
    }

    return caps;
}

static int sc_available_threads(int requested) {
#ifdef _WIN32
    if (requested > 0) {
        return requested;
    }
    return 1;
#else
    long nproc = 1;
    if (requested > 0) {
        return requested;
    }
    nproc = sysconf(_SC_NPROCESSORS_ONLN);
    if (nproc < 1) {
        nproc = 1;
    }
    if (nproc > INT_MAX) {
        nproc = INT_MAX;
    }
    return (int) nproc;
#endif
}

static int sc_choose_worker_count(const SCImportPlan *plan, int requested_threads) {
    int ncpu;
    long chunk_cap_long;
    long min_rows_per_chunk;
    int chunk_cap;
    int workers;

    ncpu = sc_available_threads(0);
    if (requested_threads > 0) {
        workers = requested_threads;
    } else {
        workers = ncpu > 1 ? (ncpu - 1) : 1;
    }

    if (plan->window_rows <= 0) {
        return 1;
    }

    min_rows_per_chunk = sc_min_rows_per_chunk(plan->window_rows);
    chunk_cap_long = plan->window_rows / min_rows_per_chunk;
    if (chunk_cap_long < 1) {
        chunk_cap_long = 1;
    }
    if (chunk_cap_long > INT_MAX) {
        chunk_cap_long = INT_MAX;
    }
    chunk_cap = (int) chunk_cap_long;

    if (workers > chunk_cap) {
        workers = chunk_cap;
    }
    if (workers < 1) {
        workers = 1;
    }

    return workers;
}

static int sc_plan_row_chunks(SCImportPlan *plan, SCChunk **chunks_out, int *nchunks_out) {
    int i;
    int nchunks;
    long base;
    long rem;
    SCChunk *chunks;

    if (plan->window_rows <= 0 || plan->n_workers <= 1) {
        return 0;
    }

    nchunks = plan->n_workers;
    if ((long) nchunks > plan->window_rows) {
        nchunks = (int) plan->window_rows;
    }
    if (nchunks <= 1) {
        return 0;
    }

    chunks = (SCChunk *) SC_CALLOC(nchunks, SCChunk);
    if (!chunks) return 0;
    base = plan->window_rows / nchunks;
    rem = plan->window_rows % nchunks;

    {
        long start = plan->window_start;
        for (i = 0; i < nchunks; ++i) {
            long size = base + (i < rem ? 1 : 0);
            chunks[i].start_row = start;
            chunks[i].row_count = size;
            start += size;
        }
    }

    *chunks_out = chunks;
    *nchunks_out = nchunks;
    return 1;
}

static void sc_set_vector_class(SEXP col, VarType var_type) {
    SEXP cls;
    switch (var_type) {
        case DECLARED_DATE:
            PROTECT(cls = Rf_mkString("Date"));
            Rf_setAttrib(col, R_ClassSymbol, cls);
            UNPROTECT(1);
            break;
        case DECLARED_TIME:
            PROTECT(cls = Rf_allocVector(STRSXP, 2));
            SET_STRING_ELT(cls, 0, Rf_mkChar("hms"));
            SET_STRING_ELT(cls, 1, Rf_mkChar("difftime"));
            Rf_setAttrib(col, R_ClassSymbol, cls);
            Rf_setAttrib(col, Rf_install("units"), Rf_mkString("secs"));
            UNPROTECT(1);
            break;
        case DECLARED_DATETIME:
            PROTECT(cls = Rf_allocVector(STRSXP, 2));
            SET_STRING_ELT(cls, 0, Rf_mkChar("POSIXct"));
            SET_STRING_ELT(cls, 1, Rf_mkChar("POSIXt"));
            Rf_setAttrib(col, R_ClassSymbol, cls);
            Rf_setAttrib(col, Rf_install("tzone"), Rf_mkString("UTC"));
            UNPROTECT(1);
            break;
        default:
            break;
    }
}

static int sc_add_na_index(SCChunkColumn *col, int pos, char tag) {
    if (col->na_index_n == col->na_index_cap) {
        if (col->na_index_cap > SIZE_MAX / sizeof(SCTaggedRow) / 2) return 0;
        size_t capacity = col->na_index_cap ? col->na_index_cap * 2 : 8;
        SCTaggedRow *rows = SC_REALLOC(col->tagged, capacity, SCTaggedRow);
        if (!rows) return 0;
        col->tagged = rows;
        col->na_index_cap = capacity;
    }

    col->tagged[col->na_index_n++] = (SCTaggedRow){pos, tag};
    return 1;
}

static int sc_probe_note_handler(int index, const char *note, void *ctx_) {
    SCParseState *state = (SCParseState *)ctx_;
    SCSchema *schema = state->schema;
    (void)index;
    if (note != NULL) {
        char **notes = SC_REALLOC(schema->notes, schema->notes_n + 1, char *);
        if (!notes) return READSTAT_HANDLER_ABORT;
        schema->notes = notes;
        notes[schema->notes_n] = sc_strdup(note);
        if (!notes[schema->notes_n]) return READSTAT_HANDLER_ABORT;
        schema->notes_n++;
    }
    return READSTAT_HANDLER_OK;
}

static void sc_attach_dataset_metadata(SEXP output, const SCSchema *schema) {
    Rf_setAttrib(output, Rf_install(ddiwr_metadata_attribute(schema->vendor)), schema->vendor_metadata);
    if (schema->notes_n > 0) {
        SEXP notes = PROTECT(Rf_allocVector(STRSXP, schema->notes_n));
        for (size_t i = 0; i < schema->notes_n; i++) {
            SET_STRING_ELT(notes, i, Rf_mkCharCE(schema->notes[i], CE_UTF8));
        }
        ddiwr_metadata_set_field(output, schema->vendor, "notes", notes);
        UNPROTECT(1);
    }
}

static int sc_probe_metadata_handler(readstat_metadata_t *metadata, void *ctx_) {
    SCParseState *state = (SCParseState *) ctx_;
    int var_count = readstat_get_var_count(metadata);
    int kept = var_count - state->cols_skip_n;
    if (readstat_attributes_copy(&state->schema->spss_attributes, metadata->spss_attributes) != READSTAT_OK) {
        return READSTAT_HANDLER_ABORT;
    }
    if (kept < 0) {
        kept = 0;
    }
    /* The metadata probe runs on the R thread, before workers start. */
    SEXP vendor_metadata = PROTECT(ddiwr_dataset_metadata(metadata, state->schema->ext));
    state->schema->vendor_metadata = vendor_metadata;
    R_PreserveObject(vendor_metadata);
    UNPROTECT(1);
    state->schema->total_rows = readstat_get_row_count(metadata);
    state->schema->cols = SC_CALLOC(kept, SCColumnSchema);
    if (kept && !state->schema->cols) return READSTAT_HANDLER_ABORT;
    state->schema->ncols = kept;
    if (readstat_get_file_label(metadata) != NULL && *readstat_get_file_label(metadata) != '\0') {
        state->schema->file_label = sc_strdup(readstat_get_file_label(metadata));
        if (!state->schema->file_label) return READSTAT_HANDLER_ABORT;
    }
    return READSTAT_HANDLER_OK;
}

static int sc_probe_variable_handler(int index, readstat_variable_t *variable, const char *val_labels, void *ctx_) {
    SCParseState *state = (SCParseState *) ctx_;
    SCSchema *schema = state->schema;
    int kept_index;
    SCColumnSchema *col;
    const char *name;
    const char *label;
    const char *format;

    if (sc_cols_skip_contains(state->cols_skip, state->cols_skip_n, index)) {
        return READSTAT_HANDLER_SKIP_VARIABLE;
    }

    kept_index = readstat_variable_get_index_after_skipping(variable);
    col = &schema->cols[kept_index];
    col->source_index = index;
    col->kept_index = kept_index;
    col->readstat_type = readstat_variable_get_type(variable);
    col->var_type = numTypeFromMetadata(schema->vendor, readstat_variable_get_format(variable),
        readstat_variable_get_informat(variable));
    name = readstat_variable_get_name(variable);
    label = readstat_variable_get_label(variable);
    format = readstat_variable_get_format(variable);
    col->name = sc_strdup(name == NULL ? "" : name);
    col->label = (label != NULL && *label != '\0') ? sc_strdup(label) : NULL;
    col->format = (format != NULL && *format != '\0') ? sc_strdup(format) : NULL;
    col->val_labels_name = (val_labels != NULL && *val_labels != '\0') ? sc_strdup(val_labels) : NULL;
    if (!col->name || (label && *label && !col->label) || (format && *format && !col->format) ||
            (val_labels && *val_labels && !col->val_labels_name)) return READSTAT_HANDLER_ABORT;
    SEXP vendor_metadata = PROTECT(ddiwr_column_metadata(variable, schema->vendor));
    col->vendor_metadata = vendor_metadata;
    R_PreserveObject(vendor_metadata);
    UNPROTECT(1);
    col->storage_width = readstat_variable_get_storage_width(variable);
    if (schema->vendor == DECLARED_SPSS && readstat_variable_get_display_width(variable) != 8) {
        col->has_display_width = 1;
        col->display_width = readstat_variable_get_display_width(variable);
    }

    {
        int n_ranges = readstat_variable_get_missing_ranges_count(variable);
        int i;
        for (i = 0; i < n_ranges; ++i) {
            readstat_value_t lo_value = readstat_variable_get_missing_range_lo(variable, i);
            readstat_value_t hi_value = readstat_variable_get_missing_range_hi(variable, i);
            if (readstat_variable_get_type(variable) == READSTAT_TYPE_STRING ||
                readstat_variable_get_type(variable) == READSTAT_TYPE_STRING_REF) {
                const char *lo = readstat_string_value(lo_value);
                const char *hi = readstat_string_value(hi_value);
                if (lo == hi) {
                    if (col->str_na_values_n < 3) {
                        col->str_na_values[col->str_na_values_n] = sc_strdup(lo == NULL ? "" : lo);
                        if (!col->str_na_values[col->str_na_values_n]) return READSTAT_HANDLER_ABORT;
                        col->str_na_values_n++;
                    }
                } else {
                    col->has_str_na_range = 1;
                    col->str_na_range[0] = sc_strdup(lo == NULL ? "" : lo);
                    col->str_na_range[1] = sc_strdup(hi == NULL ? "" : hi);
                    if (!col->str_na_range[0] || !col->str_na_range[1]) return READSTAT_HANDLER_ABORT;
                }
            } else {
                double lo = readstat_double_value(lo_value);
                double hi = readstat_double_value(hi_value);
                if (lo == hi) {
                    if (col->num_na_values_n < 3) {
                        col->num_na_values[col->num_na_values_n++] = lo;
                    }
                } else {
                    col->has_num_na_range = 1;
                    col->num_na_range[0] = lo;
                    col->num_na_range[1] = hi;
                }
            }
        }
    }
    return READSTAT_HANDLER_OK;
}

static int sc_probe_value_label_handler(const char *val_labels, readstat_value_t value, const char *label, void *ctx_) {
    SCParseState *state = (SCParseState *) ctx_;
    SCLabelSet *set;
    if (val_labels == NULL || label == NULL) {
        return READSTAT_HANDLER_OK;
    }

    switch (value.type) {
        case READSTAT_TYPE_STRING:
            set = sc_find_or_create_labelset(state->schema, val_labels, LABELSET_STRING);
            if (!set || !sc_labelset_ensure_capacity(set)) return READSTAT_HANDLER_ABORT;
            set->labels[set->n] = sc_strdup(label);
            set->svalues[set->n] = sc_strdup(readstat_string_value(value));
            set->n++;
            if (!set->labels[set->n - 1] || !set->svalues[set->n - 1]) return READSTAT_HANDLER_ABORT;
            break;
        default:
            set = sc_find_or_create_labelset(state->schema, val_labels, LABELSET_DOUBLE);
            if (!set || !sc_labelset_ensure_capacity(set)) return READSTAT_HANDLER_ABORT;
            set->labels[set->n] = sc_strdup(label);
            if (!set->labels[set->n]) return READSTAT_HANDLER_ABORT;
            if (readstat_value_is_tagged_missing(value)) {
                set->dvalues[set->n] = make_tagged_na((char) tolower((unsigned char) readstat_value_tag(value)));
            } else {
                set->dvalues[set->n] = readstat_double_value(value);
            }
            set->n++;
            break;
    }

    return READSTAT_HANDLER_OK;
}

static int sc_probe_characteristic_handler(const readstat_variable_t *variable,
        const char *name, const char *value, void *ctx_) {
    SCParseState *state = (SCParseState *)ctx_;
    return readstat_characteristic_add(&state->schema->stata_characteristics,
            variable ? variable->name : NULL, name, value) == READSTAT_OK ?
        READSTAT_HANDLER_OK : READSTAT_HANDLER_ABORT;
}

static readstat_error_t sc_probe_readstat_metadata(FileExt ext, const char *path, const char *encoding, const int *cols_skip, int cols_skip_n, SCSchema *schema) {
    readstat_error_t result;
    readstat_parser_t *parser;
    SCParseState state;
    memset(&state, 0, sizeof(state));
    memset(schema, 0, sizeof(*schema));
    schema->ext = ext;
    schema->vendor = extVendor(ext);

    parser = readstat_parser_init();
    if (parser == NULL) {
        return READSTAT_ERROR_MALLOC;
    }

    if (encoding != NULL && *encoding != '\0') {
        readstat_set_file_character_encoding(parser, encoding);
    }

    state.mode = 1;
    state.path = path;
    state.encoding = encoding;
    state.cols_skip = cols_skip;
    state.cols_skip_n = cols_skip_n;
    state.schema = schema;

    readstat_set_metadata_handler(parser, sc_probe_metadata_handler);
    readstat_set_note_handler(parser, sc_probe_note_handler);
    readstat_set_variable_handler(parser, sc_probe_variable_handler);
    readstat_set_value_label_handler(parser, sc_probe_value_label_handler);
    readstat_set_characteristic_handler(parser, sc_probe_characteristic_handler);
    if (ext == DECLARED_DTA) {
        /* Without a value handler ReadStat skips the rows and reports the full count. */
        result = readstat_parse_dta(parser, path, &state);
    } else if (ext == DECLARED_SAV) {
        result = readstat_parse_sav(parser, path, &state);
    } else {
        result = READSTAT_ERROR_PARSE;
    }
    readstat_parser_free(parser);

    if (result != READSTAT_OK) {
        sc_free_schema(schema);
        memset(schema, 0, sizeof(*schema));
        return result;
    }

    if (schema->total_rows < 0) {
        sc_free_schema(schema);
        memset(schema, 0, sizeof(*schema));
        return READSTAT_ERROR_PARSE;
    }

    return READSTAT_OK;
}

static int sc_worker_metadata_handler(readstat_metadata_t *metadata, void *ctx_) {
    (void) metadata;
    (void) ctx_;
    return READSTAT_HANDLER_OK;
}

static int sc_worker_variable_handler(int index, readstat_variable_t *variable, const char *val_labels, void *ctx_) {
    SCParseState *state = (SCParseState *) ctx_;
    (void) variable;
    (void) val_labels;
    if (sc_cols_skip_contains(state->cols_skip, state->cols_skip_n, index)) {
        return READSTAT_HANDLER_SKIP_VARIABLE;
    }
    return READSTAT_HANDLER_OK;
}

static readstat_error_t sc_chunk_result_init(const SCSchema *schema, long nrows, SCChunkResult *out) {
    int i;
    memset(out, 0, sizeof(*out));
    out->nrows = nrows;
    out->ncols = schema->ncols;
    out->cols = (SCChunkColumn *) SC_CALLOC((size_t) schema->ncols, SCChunkColumn);

    if (schema->ncols && !out->cols) return READSTAT_ERROR_MALLOC;
    for (i = 0; i < schema->ncols; ++i) {
        if (schema->cols[i].readstat_type == READSTAT_TYPE_STRING ||
            schema->cols[i].readstat_type == READSTAT_TYPE_STRING_REF) {
            out->cols[i].svalues = (char **) SC_CALLOC((size_t) nrows, char *);
            out->cols[i].smissing = (unsigned char *) SC_CALLOC((size_t) nrows, unsigned char);
            if (nrows && (!out->cols[i].svalues || !out->cols[i].smissing)) {
                sc_free_chunk_result(out);
                return READSTAT_ERROR_MALLOC;
            }
        } else {
            long j;
            out->cols[i].dvalues = (double *) SC_CALLOC((size_t) nrows, double);
            if (nrows && !out->cols[i].dvalues) {
                sc_free_chunk_result(out);
                return READSTAT_ERROR_MALLOC;
            }
            for (j = 0; j < nrows; ++j) {
                out->cols[i].dvalues[j] = NA_REAL;
            }
        }
    }

    return READSTAT_OK;
}

static int sc_worker_value_handler(int obs_index, readstat_variable_t *variable, readstat_value_t value, void *ctx_) {
    SCParseState *state = (SCParseState *) ctx_;
    int var_index = readstat_variable_get_index_after_skipping(variable);
    SCChunkColumn *col = &state->chunk_result->cols[var_index];
    SCColumnSchema *schema_col = &state->schema->cols[var_index];

    if (obs_index < 0 || obs_index >= state->chunk_result->nrows) {
        return READSTAT_HANDLER_ABORT;
    }

    if (schema_col->readstat_type == READSTAT_TYPE_STRING ||
        schema_col->readstat_type == READSTAT_TYPE_STRING_REF) {
        const char *str_value = readstat_string_value(value);
        if (readstat_value_is_tagged_missing(value) ||
            readstat_value_is_system_missing(value) ||
            readstat_value_is_defined_missing(value, variable)) {
            col->smissing[obs_index] = 1;
        } else if (str_value && *str_value) {
            col->svalues[obs_index] = sc_strdup(str_value);
            if (!col->svalues[obs_index]) return READSTAT_HANDLER_ABORT;
        }
    } else {
        if (readstat_value_is_tagged_missing(value)) {
            col->dvalues[obs_index] = NA_REAL;
            if (!sc_add_na_index(col, obs_index + 1, readstat_value_tag(value))) {
                return READSTAT_HANDLER_ABORT;
            }
        } else if (readstat_value_is_system_missing(value)) {
            col->dvalues[obs_index] = NA_REAL;
        } else {
            col->dvalues[obs_index] = adjustDatetimeToR(
                state->schema->vendor,
                schema_col->var_type,
                readstat_double_value(value)
            );
        }
    }

    return READSTAT_HANDLER_OK;
}

static readstat_value_t sc_dta_interpret_int8_bytes(const dta_ctx_t *ctx, const void *buf) {
    readstat_value_t value = { .type = READSTAT_TYPE_INT8 };
    int8_t byte = 0;
    memcpy(&byte, buf, sizeof(int8_t));
    if (ctx->machine_is_twos_complement) {
        byte = ones_to_twos_complement1(byte);
    }
    if (byte > ctx->max_int8) {
        if (ctx->supports_tagged_missing && byte > DTA_113_MISSING_INT8) {
            value.tag = 'a' + (byte - DTA_113_MISSING_INT8_A);
            value.is_tagged_missing = 1;
        } else {
            value.is_system_missing = 1;
        }
    }
    value.v.i8_value = byte;
    return value;
}

static readstat_value_t sc_dta_interpret_int16_bytes(const dta_ctx_t *ctx, const void *buf) {
    readstat_value_t value = { .type = READSTAT_TYPE_INT16 };
    int16_t num = 0;
    memcpy(&num, buf, sizeof(int16_t));
    if (ctx->bswap) {
        num = byteswap2(num);
    }
    if (ctx->machine_is_twos_complement) {
        num = ones_to_twos_complement2(num);
    }
    if (num > ctx->max_int16) {
        if (ctx->supports_tagged_missing && num > DTA_113_MISSING_INT16) {
            value.tag = 'a' + (num - DTA_113_MISSING_INT16_A);
            value.is_tagged_missing = 1;
        } else {
            value.is_system_missing = 1;
        }
    }
    value.v.i16_value = num;
    return value;
}

static readstat_value_t sc_dta_interpret_int32_bytes(const dta_ctx_t *ctx, const void *buf) {
    readstat_value_t value = { .type = READSTAT_TYPE_INT32 };
    int32_t num = 0;
    memcpy(&num, buf, sizeof(int32_t));
    if (ctx->bswap) {
        num = byteswap4(num);
    }
    if (ctx->machine_is_twos_complement) {
        num = ones_to_twos_complement4(num);
    }
    if (num > ctx->max_int32) {
        if (ctx->supports_tagged_missing && num > DTA_113_MISSING_INT32) {
            value.tag = 'a' + (num - DTA_113_MISSING_INT32_A);
            value.is_tagged_missing = 1;
        } else {
            value.is_system_missing = 1;
        }
    }
    value.v.i32_value = num;
    return value;
}

static readstat_value_t sc_dta_interpret_float_bytes(const dta_ctx_t *ctx, const void *buf) {
    readstat_value_t value = { .type = READSTAT_TYPE_FLOAT };
    float f_num = NAN;
    int32_t num = 0;
    memcpy(&num, buf, sizeof(int32_t));
    if (ctx->bswap) {
        num = byteswap4(num);
    }
    if (num > ctx->max_float) {
        if (ctx->supports_tagged_missing && num > DTA_113_MISSING_FLOAT) {
            value.tag = 'a' + ((num - DTA_113_MISSING_FLOAT_A) >> 11);
            value.is_tagged_missing = 1;
        } else {
            value.is_system_missing = 1;
        }
    } else {
        memcpy(&f_num, &num, sizeof(int32_t));
    }
    value.v.float_value = f_num;
    return value;
}

static readstat_value_t sc_dta_interpret_double_bytes(const dta_ctx_t *ctx, const void *buf) {
    readstat_value_t value = { .type = READSTAT_TYPE_DOUBLE };
    double d_num = NAN;
    int64_t num = 0;
    memcpy(&num, buf, sizeof(int64_t));
    if (ctx->bswap) {
        num = byteswap8(num);
    }
    if (num > ctx->max_double) {
        if (ctx->supports_tagged_missing && num > DTA_113_MISSING_DOUBLE) {
            value.tag = 'a' + ((num - DTA_113_MISSING_DOUBLE_A) >> 40);
            value.is_tagged_missing = 1;
        } else {
            value.is_system_missing = 1;
        }
    } else {
        memcpy(&d_num, &num, sizeof(int64_t));
    }
    value.v.double_value = d_num;
    return value;
}

static readstat_error_t sc_dta_store_value(
    const dta_ctx_t *ctx,
    const SCSchema *schema,
    SCChunkResult *result,
    long row_index,
    const readstat_variable_t *variable,
    readstat_value_t value
) {
    int kept_index = variable->index_after_skipping;
    SCChunkColumn *col;
    SCColumnSchema *schema_col;

    if (kept_index < 0 || kept_index >= result->ncols) {
        return READSTAT_ERROR_PARSE;
    }

    col = &result->cols[kept_index];
    schema_col = &((SCSchema *) schema)->cols[kept_index];

    if (schema_col->readstat_type == READSTAT_TYPE_STRING ||
        schema_col->readstat_type == READSTAT_TYPE_STRING_REF) {
        const char *str_value = readstat_string_value(value);
        if (readstat_value_is_tagged_missing(value) ||
            readstat_value_is_system_missing(value) ||
            readstat_value_is_defined_missing(value, (readstat_variable_t *) variable)) {
            col->smissing[row_index] = 1;
        } else if (str_value && *str_value) {
            col->svalues[row_index] = sc_strdup(str_value);
            if (!col->svalues[row_index]) return READSTAT_ERROR_MALLOC;
        }
    } else {
        if (readstat_value_is_tagged_missing(value)) {
            col->dvalues[row_index] = NA_REAL;
            if (!sc_add_na_index(col, (int)row_index + 1, readstat_value_tag(value))) {
                return READSTAT_ERROR_MALLOC;
            }
        } else if (readstat_value_is_system_missing(value)) {
            col->dvalues[row_index] = NA_REAL;
        } else {
            col->dvalues[row_index] = adjustDatetimeToR(
                schema->vendor,
                schema_col->var_type,
                readstat_double_value(value)
            );
        }
    }

    return READSTAT_OK;
}

static readstat_error_t sc_dta_decode_row(
    const unsigned char *buf,
    const dta_ctx_t *ctx,
    const SCSchema *schema,
    SCChunkResult *result,
    long row_index,
    char *str_buf,
    size_t str_buf_len,
    iconv_t converter
) {
    int j;
    readstat_off_t offset = 0;

    for (j = 0; j < ctx->nvar; j++) {
        size_t max_len;
        readstat_value_t value;
        readstat_error_t retval;
        memset(&value, 0, sizeof(value));

        retval = dta_type_info(ctx->typlist[j], (dta_ctx_t *) ctx, &max_len, &value.type);
        if (retval != READSTAT_OK) {
            return retval;
        }

        if (ctx->variables[j]->skip) {
            offset += max_len;
            continue;
        }
        if (offset + max_len > ctx->record_len) {
            return READSTAT_ERROR_PARSE;
        }

        if (value.type == READSTAT_TYPE_STRING) {
            size_t str_len;
            if (max_len == 0) {
                return READSTAT_ERROR_PARSE;
            }
            str_len = strnlen((const char *) &buf[offset], max_len);
            retval = readstat_convert_notrim(str_buf, str_buf_len, (const char *) &buf[offset], str_len, converter);
            if (retval != READSTAT_OK) {
                return retval;
            }
            value.v.string_value = str_buf;
        } else if (value.type == READSTAT_TYPE_STRING_REF) {
            value.v.string_value = dta_lookup_string_ref((dta_ctx_t *) ctx, &buf[offset]);
            value.type = READSTAT_TYPE_STRING;
        } else if (value.type == READSTAT_TYPE_INT8) {
            value = sc_dta_interpret_int8_bytes(ctx, &buf[offset]);
        } else if (value.type == READSTAT_TYPE_INT16) {
            value = sc_dta_interpret_int16_bytes(ctx, &buf[offset]);
        } else if (value.type == READSTAT_TYPE_INT32) {
            value = sc_dta_interpret_int32_bytes(ctx, &buf[offset]);
        } else if (value.type == READSTAT_TYPE_FLOAT) {
            value = sc_dta_interpret_float_bytes(ctx, &buf[offset]);
        } else if (value.type == READSTAT_TYPE_DOUBLE) {
            value = sc_dta_interpret_double_bytes(ctx, &buf[offset]);
        }

        retval = sc_dta_store_value(ctx, schema, result, row_index, ctx->variables[j], value);
        if (retval != READSTAT_OK) {
            return retval;
        }

        offset += max_len;
    }

    return READSTAT_OK;
}

static readstat_error_t sc_worker_read_chunk_custom_dta(SCWorkerState *worker) {
    const dta_ctx_t *ctx = worker->dta_ctx;
    FILE *fp;
    unsigned char *buf;
    char *str_buf = NULL;
    size_t str_buf_len = 1;
    long i;
    readstat_error_t result = READSTAT_OK;

    if (ctx == NULL) {
        return READSTAT_ERROR_PARSE;
    }

    result = sc_chunk_result_init(worker->schema, worker->chunk.row_count, &worker->result_data);
    if (result != READSTAT_OK) {
        return result;
    }

    for (i = 0; i < ctx->nvar; ++i) {
        if (!ctx->variables[i]->skip &&
            ctx->variables[i]->type == READSTAT_TYPE_STRING &&
            4 * ctx->variables[i]->storage_width + 1 > str_buf_len) {
            str_buf_len = 4 * ctx->variables[i]->storage_width + 1;
        }
    }

    iconv_t converter = NULL;
    if (ctx->converter) {
        const char *encoding = worker->encoding && *worker->encoding ?
                worker->encoding : "WINDOWS-1252";
        converter = iconv_open("UTF-8", encoding);
        if (converter == (iconv_t)-1) return READSTAT_ERROR_UNSUPPORTED_CHARSET;
    }

    buf = (unsigned char *) SC_CALLOC(ctx->record_len, unsigned char);
    str_buf = (char *) SC_CALLOC(str_buf_len, char);
    fp = fopen(worker->path, "rb");
    if (!buf || !str_buf) {
        result = READSTAT_ERROR_MALLOC;
        goto cleanup;
    }
    if (fp == NULL) {
        result = READSTAT_ERROR_OPEN;
        goto cleanup;
    }
    if (fseeko(fp, (off_t) (worker->dta_rows_offset + (int64_t) ctx->record_len * worker->chunk.start_row), SEEK_SET) != 0) {
        result = READSTAT_ERROR_SEEK;
        goto cleanup;
    }

    for (i = 0; i < worker->chunk.row_count; ++i) {
        if (fread(buf, 1, ctx->record_len, fp) != ctx->record_len) {
            result = READSTAT_ERROR_READ;
            goto cleanup;
        }
        result = sc_dta_decode_row(buf, ctx, worker->schema, &worker->result_data, i, str_buf, str_buf_len, converter);
        if (result != READSTAT_OK) {
            goto cleanup;
        }
    }

cleanup:
    if (converter) iconv_close(converter);
    if (fp != NULL) {
        fclose(fp);
    }
    if (buf != NULL) {
        SC_FREE(buf);
    }
    if (str_buf != NULL) {
        SC_FREE(str_buf);
    }
    if (result != READSTAT_OK) {
        sc_free_chunk_result(&worker->result_data);
    }
    return result;
}

static readstat_error_t sc_worker_read_chunk(SCWorkerState *worker) {
    readstat_error_t result;
    readstat_parser_t *parser;
    SCParseState state;

    if (worker->use_custom_dta) {
        return sc_worker_read_chunk_custom_dta(worker);
    }

    memset(&state, 0, sizeof(state));
    result = sc_chunk_result_init(worker->schema, worker->chunk.row_count, &worker->result_data);
    if (result != READSTAT_OK) {
        return result;
    }

    parser = readstat_parser_init();
    if (parser == NULL) {
        sc_free_chunk_result(&worker->result_data);
        return READSTAT_ERROR_MALLOC;
    }

    if (worker->encoding != NULL && *worker->encoding != '\0') {
        readstat_set_file_character_encoding(parser, worker->encoding);
    }

    state.mode = 2;
    state.path = worker->path;
    state.encoding = worker->encoding;
    state.cols_skip = worker->cols_skip;
    state.cols_skip_n = worker->cols_skip_n;
    state.schema = (SCSchema *) worker->schema;
    state.chunk_result = &worker->result_data;

    readstat_set_metadata_handler(parser, sc_worker_metadata_handler);
    readstat_set_variable_handler(parser, sc_worker_variable_handler);
    readstat_set_value_handler(parser, sc_worker_value_handler);
    readstat_set_row_offset(parser, worker->chunk.start_row);
    readstat_set_row_limit(parser, worker->chunk.row_count);

    result = readstat_parse_dta(parser, worker->path, &state);
    readstat_parser_free(parser);

    if (result != READSTAT_OK) {
        sc_free_chunk_result(&worker->result_data);
    }

    return result;
}

#ifndef _WIN32
static void *sc_worker_thread_main(void *arg) {
    SCWorkerState *worker = (SCWorkerState *) arg;
    worker->error = sc_worker_read_chunk(worker);
    if (worker->error != READSTAT_OK) {
        snprintf(worker->error_message, sizeof(worker->error_message), "%s", readstat_error_message(worker->error));
    }
    return NULL;
}
#endif

static SEXP sc_labelset_to_sexp(const SCLabelSet *set) {
    size_t i;
    SEXP out;
    SEXP names;

    if (set->type == LABELSET_STRING) {
        PROTECT(out = Rf_allocVector(STRSXP, (R_xlen_t) set->n));
        for (i = 0; i < set->n; ++i) {
            SET_STRING_ELT(out, i, Rf_mkCharCE(set->svalues[i], CE_UTF8));
        }
        PROTECT(names = Rf_allocVector(STRSXP, (R_xlen_t) set->n));
        for (i = 0; i < set->n; ++i) {
            SET_STRING_ELT(names, i, Rf_mkCharCE(set->labels[i], CE_UTF8));
        }
        Rf_setAttrib(out, R_NamesSymbol, names);
        UNPROTECT(2);
        return out;
    }

    PROTECT(out = Rf_allocVector(REALSXP, (R_xlen_t) set->n));
    for (i = 0; i < set->n; ++i) {
        char tag = tagged_na_value(set->dvalues[i]);
        if (ISNAN(set->dvalues[i]) && tag != '\0') {
            REAL(out)[i] = sc_tag_code_value(tag);
        } else {
            REAL(out)[i] = set->dvalues[i];
        }
    }

    PROTECT(names = Rf_allocVector(STRSXP, (R_xlen_t) set->n));
    for (i = 0; i < set->n; ++i) {
        SET_STRING_ELT(names, i, Rf_mkCharCE(set->labels[i], CE_UTF8));
    }
    Rf_setAttrib(out, R_NamesSymbol, names);
    UNPROTECT(2);
    return out;
}

static void sc_finalize_tagged_missing_attrs(const SCOutput *data, int column, SEXP col) {
    R_xlen_t total = 0, at = 0;
    char tags[26];
    int seen[26] = {0}, unique = 0;
    for (int k = 0; k < data->count; k++)
        total += data->chunks[k].decoded->cols[column].na_index_n;
    if (total == 0) return;

    SEXP index = PROTECT(Rf_allocVector(INTSXP, total));
    SEXP names = PROTECT(Rf_allocVector(STRSXP, total));
    for (int k = 0; k < data->count; k++) {
        const SCChunk *chunk = &data->chunks[k];
        const SCChunkColumn *source = &chunk->decoded->cols[column];
        long offset = chunk->start_row - data->chunks[0].start_row;
        for (size_t j = 0; j < source->na_index_n; j++, at++) {
            char tag = source->tagged[j].tag;
            char lower = (char)tolower((unsigned char)tag);
            INTEGER(index)[at] = (int)(offset + source->tagged[j].position);
            SET_STRING_ELT(names, at, sc_tag_code_name(tag));
            if (lower >= 'a' && lower <= 'z' && !seen[lower - 'a']) {
                seen[lower - 'a'] = 1;
                tags[unique++] = lower;
            }
        }
    }

    Rf_setAttrib(index, R_NamesSymbol, names);
    Rf_setAttrib(col, Rf_install("na_index"), index);
    SEXP values = PROTECT(Rf_allocVector(REALSXP, unique));
    for (int i = 0; i < unique; i++) REAL(values)[i] = sc_tag_code_value(tags[i]);
    Rf_setAttrib(col, Rf_install("na_values"), values);
    UNPROTECT(3);
}

static void sc_output_cleanup(void *context) {
    SCOutput *data = context;
    for (int i = 0; i < data->count; i++)
        sc_free_chunk_result(data->chunks[i].decoded);
    SC_FREE(data->workers);
    SC_FREE(data->chunks);
    sc_free_schema(data->schema);
}

static SEXP sc_build_r_output(void *context) {
    const SCOutput *data = context;
    const SCSchema *schema = data->schema;
    int user_na = data->user_na;
    long nrows = 0;
    for (int k = 0; k < data->count; k++) nrows += data->chunks[k].row_count;
    int i;
    SEXP output;
    SEXP names;
    SEXP row_names;

    PROTECT(output = Rf_allocVector(VECSXP, schema->ncols));
    PROTECT(names = Rf_allocVector(STRSXP, schema->ncols));

    for (i = 0; i < schema->ncols; ++i) {
        const SCColumnSchema *col_schema = &schema->cols[i];
        int strings = col_schema->readstat_type == READSTAT_TYPE_STRING ||
                col_schema->readstat_type == READSTAT_TYPE_STRING_REF;
        SEXP col = PROTECT(Rf_allocVector(strings ? STRSXP : REALSXP, nrows));
        SET_STRING_ELT(names, i, Rf_mkCharCE(col_schema->name == NULL ? "" : col_schema->name, CE_UTF8));

        for (int k = 0; k < data->count; k++) {
            const SCChunk *chunk = &data->chunks[k];
            SCChunkColumn *source = &chunk->decoded->cols[i];
            long start = chunk->start_row - data->chunks[0].start_row;
            if (strings) {
                for (long r = 0; r < chunk->row_count; r++) {
                    const char *text = source->svalues[r];
                    SET_STRING_ELT(col, start + r, source->smissing[r] ? NA_STRING :
                        Rf_mkCharCE(text ? text : "", CE_UTF8));
                    SC_FREE(source->svalues[r]);
                    source->svalues[r] = NULL;
                }
                SC_FREE(source->svalues);
                source->svalues = NULL;
                SC_FREE(source->smissing);
                source->smissing = NULL;
            } else {
                memcpy(REAL(col) + start, source->dvalues, (size_t)chunk->row_count * sizeof(double));
                SC_FREE(source->dvalues);
                source->dvalues = NULL;
            }
        }

        if (col_schema->label != NULL) {
            Rf_setAttrib(col, Rf_install("label"), Rf_mkString(col_schema->label));
        }
        Rf_setAttrib(col, Rf_install(ddiwr_metadata_attribute(schema->vendor)), col_schema->vendor_metadata);
        if (user_na && col_schema->num_na_values_n > 0) {
            SEXP na_values = PROTECT(Rf_allocVector(REALSXP, col_schema->num_na_values_n));
            int k;
            for (k = 0; k < col_schema->num_na_values_n; ++k) {
                REAL(na_values)[k] = col_schema->num_na_values[k];
            }
            Rf_setAttrib(col, Rf_install("na_values"), na_values);
            UNPROTECT(1);
        }
        if (user_na && col_schema->has_num_na_range) {
            SEXP na_range = PROTECT(Rf_allocVector(REALSXP, 2));
            REAL(na_range)[0] = col_schema->num_na_range[0];
            REAL(na_range)[1] = col_schema->num_na_range[1];
            Rf_setAttrib(col, Rf_install("na_range"), na_range);
            UNPROTECT(1);
        }
        if (user_na && col_schema->str_na_values_n > 0) {
            int k;
            SEXP na_values = PROTECT(Rf_allocVector(STRSXP, col_schema->str_na_values_n));
            for (k = 0; k < col_schema->str_na_values_n; ++k) {
                SET_STRING_ELT(na_values, k, Rf_mkCharCE(col_schema->str_na_values[k], CE_UTF8));
            }
            Rf_setAttrib(col, Rf_install("na_values"), na_values);
            UNPROTECT(1);
        }
        if (user_na && col_schema->has_str_na_range) {
            SEXP na_range = PROTECT(Rf_allocVector(STRSXP, 2));
            SET_STRING_ELT(na_range, 0, Rf_mkCharCE(col_schema->str_na_range[0], CE_UTF8));
            SET_STRING_ELT(na_range, 1, Rf_mkCharCE(col_schema->str_na_range[1], CE_UTF8));
            Rf_setAttrib(col, Rf_install("na_range"), na_range);
            UNPROTECT(1);
        }
        if (col_schema->var_type != DECLARED_DEFAULT) {
            sc_set_vector_class(col, col_schema->var_type);
        }
        if (col_schema->val_labels_name != NULL) {
            SCLabelSet *set = sc_find_labelset(schema, col_schema->val_labels_name);
            if (set != NULL) {
                SEXP labels = PROTECT(sc_labelset_to_sexp(set));
                Rf_setAttrib(col, Rf_install("labels"), labels);
                UNPROTECT(1);
            }
        }
        sc_finalize_tagged_missing_attrs(data, i, col);
        SET_VECTOR_ELT(output, i, col);
        UNPROTECT(1);
    }

    sc_attach_dataset_metadata(output, schema);

    if (schema->file_label != NULL) {
        Rf_setAttrib(output, Rf_install("label"), Rf_mkString(schema->file_label));
    }

    Rf_setAttrib(output, R_NamesSymbol, names);
    ddiwr_attach_spss_attributes(output, names, schema->spss_attributes);
    ddiwr_attach_stata_characteristics(output, names, schema->stata_characteristics);
    PROTECT(row_names = Rf_allocVector(INTSXP, 2));
    INTEGER(row_names)[0] = NA_INTEGER;
    INTEGER(row_names)[1] = -(int) nrows;
    Rf_setAttrib(output, R_RowNamesSymbol, row_names);
    Rf_setAttrib(output, R_ClassSymbol, Rf_mkString("data.frame"));
    UNPROTECT(3);
    return output;
}

static SEXP sc_read_single_fallback(SEXP spec, SEXP encoding, SEXP cols_skip, SEXP n_max, SEXP rows_skip) {
    return declared_df_parse_dta_file(spec, encoding, cols_skip, n_max, rows_skip);
}

static SEXP sc_read_parallel(SEXP spec, SEXP encoding, SEXP cols_skip_sexp, SEXP n_max, SEXP rows_skip, SEXP num_threads) {
    const char *path;
    const char *encoding_c = NULL;
    int *cols_skip = NULL;
    int cols_skip_n = 0;
    SCSchema schema;
    SCImportPlan plan;
    SCChunk *chunks = NULL;
    int nchunks = 0;
    SCWorkerState *workers = NULL;
    ddiwr_dta_prepared_t prepared;
    readstat_error_t err = READSTAT_OK;
    int i;
    long requested_skip = 0;
    long requested_nmax = -1;
    int requested_threads = 0;

    if (!sc_spec_path_cstr(spec, &path)) {
        return sc_read_single_fallback(spec, encoding, cols_skip_sexp, n_max, rows_skip);
    }
    if (!Rf_isNull(encoding)) {
        encoding_c = CHAR(STRING_ELT(encoding, 0));
    }
    if (Rf_isInteger(cols_skip_sexp) || Rf_isReal(cols_skip_sexp)) {
        cols_skip_n = Rf_length(cols_skip_sexp);
        if (cols_skip_n > 0) {
            cols_skip = (int *) SC_CALLOC(cols_skip_n, int);
            for (i = 0; i < cols_skip_n; ++i) {
                cols_skip[i] = TYPEOF(cols_skip_sexp) == INTSXP ?
                    INTEGER(cols_skip_sexp)[i] :
                    (int) REAL(cols_skip_sexp)[i];
            }
        }
    }

    if (!Rf_isNull(rows_skip)) {
        requested_skip = Rf_asInteger(rows_skip);
        if (requested_skip < 0) {
            requested_skip = 0;
        }
    }
    if (!Rf_isNull(n_max)) {
        requested_nmax = Rf_asInteger(n_max);
    }
    if (!Rf_isNull(num_threads)) {
        requested_threads = Rf_asInteger(num_threads);
    }

    memset(&schema, 0, sizeof(schema));
    memset(&plan, 0, sizeof(plan));
    memset(&prepared, 0, sizeof(prepared));

    plan.ext = DECLARED_DTA;
    plan.user_na = 1;
    plan.requested_skip = requested_skip;
    plan.requested_nmax = requested_nmax;
    plan.caps = sc_get_format_capabilities(DECLARED_DTA);

    err = sc_probe_readstat_metadata(DECLARED_DTA, path, encoding_c, cols_skip, cols_skip_n, &schema);
    if (err != READSTAT_OK) {
        if (cols_skip != NULL) {
            SC_FREE(cols_skip);
        }
        return sc_read_single_fallback(spec, encoding, cols_skip_sexp, n_max, rows_skip);
    }

    schema.ncols = schema.ncols == 0 ? 0 : schema.ncols;
    if (schema.ncols == 0) {
        sc_free_schema(&schema);
        if (cols_skip != NULL) {
            SC_FREE(cols_skip);
        }
        return sc_read_single_fallback(spec, encoding, cols_skip_sexp, n_max, rows_skip);
    }

    plan.total_rows = schema.total_rows;
    if (plan.total_rows < 0 || requested_skip > plan.total_rows) {
        sc_free_schema(&schema);
        if (cols_skip != NULL) {
            SC_FREE(cols_skip);
        }
        return sc_read_single_fallback(spec, encoding, cols_skip_sexp, n_max, rows_skip);
    }
    plan.window_start = requested_skip;
    plan.window_rows = plan.total_rows - requested_skip;
    if (requested_nmax == 0) {
        sc_free_schema(&schema);
        if (cols_skip != NULL) {
            SC_FREE(cols_skip);
        }
        return sc_read_single_fallback(spec, encoding, cols_skip_sexp, n_max, rows_skip);
    }
    if (requested_nmax > 0 && requested_nmax < plan.window_rows) {
        plan.window_rows = requested_nmax;
    }

    plan.n_workers = sc_choose_worker_count(&plan, requested_threads);

    if (!plan.caps.parallel_supported_now ||
        !plan.caps.row_count_known ||
        !plan.caps.efficient_row_chunks ||
        !sc_plan_row_chunks(&plan, &chunks, &nchunks)) {
        sc_free_schema(&schema);
        if (cols_skip != NULL) {
            SC_FREE(cols_skip);
        }
        return sc_read_single_fallback(spec, encoding, cols_skip_sexp, n_max, rows_skip);
    }

    err = dta_prepare(path, encoding_c, &prepared);
    if (err != READSTAT_OK) {
        sc_free_schema(&schema);
        if (cols_skip != NULL) {
            SC_FREE(cols_skip);
        }
        SC_FREE(chunks);
        return sc_read_single_fallback(spec, encoding, cols_skip_sexp, n_max, rows_skip);
    }
    {
        int kept_index = 0;
        for (i = 0; i < prepared.ctx->nvar; ++i) {
            if (sc_cols_skip_contains(cols_skip, cols_skip_n, i)) {
                prepared.ctx->variables[i]->skip = 1;
                prepared.ctx->variables[i]->index_after_skipping = -1;
            } else {
                prepared.ctx->variables[i]->skip = 0;
                prepared.ctx->variables[i]->index_after_skipping = kept_index++;
            }
        }
    }

    workers = (SCWorkerState *) SC_CALLOC(nchunks, SCWorkerState);
    if (!workers) {
        dta_prepared_free(&prepared);
        sc_free_schema(&schema);
        SC_FREE(chunks);
        SC_FREE(cols_skip);
        return sc_read_single_fallback(spec, encoding, cols_skip_sexp, n_max, rows_skip);
    }

    for (i = 0; i < nchunks; ++i) {
        memset(&workers[i], 0, sizeof(SCWorkerState));
        workers[i].plan = &plan;
        workers[i].schema = &schema;
        workers[i].path = path;
        workers[i].encoding = encoding_c;
        workers[i].cols_skip = cols_skip;
        workers[i].cols_skip_n = cols_skip_n;
        workers[i].chunk = chunks[i];
        workers[i].dta_ctx = prepared.ctx;
        workers[i].dta_rows_offset = prepared.rows_offset;
        workers[i].use_custom_dta = 1;
    }

#ifdef _WIN32
    for (i = 0; i < nchunks; ++i) {
        workers[i].error = sc_worker_read_chunk(&workers[i]);
        if (workers[i].error != READSTAT_OK) {
            break;
        }
    }
#else
    for (i = 0; i < nchunks; ++i) {
        if (pthread_create(&workers[i].thread, NULL, sc_worker_thread_main, &workers[i]) != 0) {
            workers[i].error = READSTAT_ERROR_MALLOC;
            snprintf(workers[i].error_message, sizeof(workers[i].error_message), "Failed to create reader thread");
            break;
        }
        workers[i].thread_started = 1;
    }
    {
        int j;
        for (j = 0; j < nchunks; ++j) {
            if (workers[j].thread_started) {
                pthread_join(workers[j].thread, NULL);
            }
        }
    }
#endif

    for (i = 0; i < nchunks; ++i) {
        if (workers[i].error != READSTAT_OK) {
            int j;
            for (j = 0; j < nchunks; ++j) {
                sc_free_chunk_result(&workers[j].result_data);
            }
            SC_FREE(workers);
            SC_FREE(chunks);
            dta_prepared_free(&prepared);
            sc_free_schema(&schema);
            if (cols_skip != NULL) {
                SC_FREE(cols_skip);
            }
            return sc_read_single_fallback(spec, encoding, cols_skip_sexp, n_max, rows_skip);
        }
    }

    for (i = 0; i < nchunks; i++) chunks[i].decoded = &workers[i].result_data;
    dta_prepared_free(&prepared);
    SC_FREE(cols_skip);
    SCOutput output = {&schema, chunks, nchunks, 1, workers};
    return R_ExecWithCleanup(sc_build_r_output, &output, sc_output_cleanup, &output);
}

SEXP declared_df_parse_dta_file_parallel(SEXP spec, SEXP encoding, SEXP cols_skip, SEXP n_max, SEXP rows_skip, SEXP num_threads) {
    return sc_read_parallel(spec, encoding, cols_skip, n_max, rows_skip, num_threads);
}

static int sc_sav_read_more(FILE *fp, unsigned char *buffer, size_t buffer_size, size_t *buffer_used, long long *buffer_base_offset) {
    long long pos = (long long) ftell(fp);
    size_t nread;
    if (pos < 0) {
        return 0;
    }
    nread = fread(buffer, 1, buffer_size, fp);
    if (nread == 0) {
        return 0;
    }
    *buffer_base_offset = pos;
    *buffer_used = nread;
    return 1;
}

static int sc_sav_collect_checkpoints(
    const char *path,
    const ddiwr_sav_row_stream_info_t *info,
    const SCChunk *chunks,
    int nchunks,
    SCSavCheckpoint *checkpoints
) {
    FILE *fp = NULL;
    unsigned char *row_buffer = NULL;
    unsigned char input_buffer[65536];
    struct sav_row_stream_s state;
    size_t buffer_used = 0;
    long long buffer_base_offset = 0;
    long current_row = 0;
    int next_checkpoint = 1;
    size_t row_out_offset = 0;

    memset(&state, 0, sizeof(state));
    fp = fopen(path, "rb");
    if (fp == NULL) {
        return 0;
    }
    if (fseeko(fp, (off_t) info->data_offset, SEEK_SET) != 0) {
        fclose(fp);
        return 0;
    }

    row_buffer = (unsigned char *) SC_CALLOC(info->row_width, unsigned char);
    state.missing_value = info->missing_double;
    state.bias = info->bias;
    state.bswap = info->bswap;
    checkpoints[0].row_start = chunks[0].start_row;
    checkpoints[0].file_offset = (long long) info->data_offset;
    memset(checkpoints[0].chunk, 0, sizeof(checkpoints[0].chunk));
    checkpoints[0].state_i = 0;

    while (current_row < info->record_count && next_checkpoint < nchunks) {
        if (buffer_used == 0 || state.status == SAV_ROW_STREAM_NEED_DATA) {
            if (!sc_sav_read_more(fp, input_buffer, sizeof(input_buffer), &buffer_used, &buffer_base_offset)) {
                SC_FREE(row_buffer);
                fclose(fp);
                return 0;
            }
            state.next_in = input_buffer;
            state.avail_in = buffer_used;
            state.status = SAV_ROW_STREAM_HAVE_DATA;
        }

        state.next_out = row_buffer + row_out_offset;
        state.avail_out = info->row_width - row_out_offset;
        sav_decompress_row(&state);
        buffer_used = state.avail_in;
        row_out_offset = info->row_width - state.avail_out;

        if (state.status == SAV_ROW_STREAM_FINISHED_ROW) {
            long long next_input_offset = buffer_base_offset + (long long) (state.next_in - input_buffer);
            current_row++;
            row_out_offset = 0;
            if (current_row == chunks[next_checkpoint].start_row) {
                checkpoints[next_checkpoint].row_start = current_row;
                checkpoints[next_checkpoint].file_offset = next_input_offset;
                memcpy(checkpoints[next_checkpoint].chunk, state.chunk, sizeof(state.chunk));
                checkpoints[next_checkpoint].state_i = state.i;
                next_checkpoint++;
            }
        } else if (state.status == SAV_ROW_STREAM_FINISHED_ALL) {
            break;
        }
    }

    SC_FREE(row_buffer);
    fclose(fp);
    return next_checkpoint == nchunks;
}

static int sc_sav_decode_chunk_rows(SCSavWorkerState *worker) {
    FILE *fp = NULL;
    unsigned char input_buffer[65536];
    size_t buffer_used = 0;
    long long buffer_base_offset = 0;
    struct sav_row_stream_s state;
    long row_index = 0;
    size_t row_out_offset = 0;

    memset(&state, 0, sizeof(state));
    worker->result.nrows = worker->row_count;
    worker->result.row_width = worker->info.row_width;
    worker->result.rows = (unsigned char *) SC_CALLOC((size_t) worker->row_count * worker->info.row_width, unsigned char);

    fp = fopen(worker->path, "rb");
    if (fp == NULL) {
        return 0;
    }
    if (fseeko(fp, (off_t) worker->checkpoint.file_offset, SEEK_SET) != 0) {
        fclose(fp);
        return 0;
    }

    state.missing_value = worker->info.missing_double;
    state.bias = worker->info.bias;
    state.bswap = worker->info.bswap;
    memcpy(state.chunk, worker->checkpoint.chunk, sizeof(state.chunk));
    state.i = worker->checkpoint.state_i;
    state.status = SAV_ROW_STREAM_NEED_DATA;

    while (row_index < worker->row_count) {
        if (buffer_used == 0 || state.status == SAV_ROW_STREAM_NEED_DATA) {
            if (!sc_sav_read_more(fp, input_buffer, sizeof(input_buffer), &buffer_used, &buffer_base_offset)) {
                SC_FREE(worker->result.rows);
                worker->result.rows = NULL;
                fclose(fp);
                return 0;
            }
            state.next_in = input_buffer;
            state.avail_in = buffer_used;
            state.status = SAV_ROW_STREAM_HAVE_DATA;
        }

        state.next_out = worker->result.rows + ((size_t) row_index * worker->info.row_width) + row_out_offset;
        state.avail_out = worker->info.row_width - row_out_offset;
        sav_decompress_row(&state);
        buffer_used = state.avail_in;
        row_out_offset = worker->info.row_width - state.avail_out;

        if (state.status == SAV_ROW_STREAM_FINISHED_ROW) {
            row_index++;
            row_out_offset = 0;
        } else if (state.status == SAV_ROW_STREAM_FINISHED_ALL) {
            break;
        }
    }

    fclose(fp);
    worker->actual_rows = row_index;
    return row_index == worker->row_count;
}

static int sc_sav_decode_chunk_to_columns(SCSavWorkerState *worker) {
    FILE *fp = NULL;
    unsigned char input_buffer[65536];
    unsigned char *row_buffer = NULL;
    size_t buffer_used = 0;
    long long buffer_base_offset = 0;
    struct sav_row_stream_s state;
    long row_index = 0;
    size_t row_out_offset = 0;
    const sav_ctx_t *ctx = worker->sav_ctx;
    iconv_t converter = (iconv_t) 0;
    char *raw_string = NULL;
    char *utf8_string = NULL;
    size_t raw_string_len = 256 + sizeof(SAV_EIGHT_SPACES) - 2;
    size_t utf8_string_len = 4 * 256 + 1 + sizeof(SAV_EIGHT_SPACES) - 2;
    int i;

    for (i = 0; i < ctx->var_index;) {
        spss_varinfo_t *info = ctx->varinfo[i];
        if ((size_t) info->string_length + sizeof(SAV_EIGHT_SPACES) - 2 > raw_string_len) {
            raw_string_len = (size_t) info->string_length + sizeof(SAV_EIGHT_SPACES) - 2;
            utf8_string_len = 4 * (size_t) info->string_length + 1 + sizeof(SAV_EIGHT_SPACES) - 2;
        }
        i += info->n_segments;
    }

    memset(&state, 0, sizeof(state));
    if (sc_chunk_result_init(worker->schema, worker->row_count, &worker->decoded) != READSTAT_OK) {
        return 0;
    }
    row_buffer = (unsigned char *) SC_CALLOC(worker->info.row_width, unsigned char);
    raw_string = (char *) SC_CALLOC(raw_string_len, char);
    utf8_string = (char *) SC_CALLOC(utf8_string_len, char);

    if (ctx->input_encoding != NULL && *ctx->input_encoding != '\0') {
        converter = iconv_open("UTF-8", ctx->input_encoding);
        if (converter == (iconv_t) -1) {
            SC_FREE(row_buffer);
            SC_FREE(raw_string);
            SC_FREE(utf8_string);
            sc_free_chunk_result(&worker->decoded);
            return 0;
        }
    }

    fp = fopen(worker->path, "rb");
    if (fp == NULL) {
        if (converter && converter != (iconv_t) -1) iconv_close(converter);
        SC_FREE(row_buffer);
        SC_FREE(raw_string);
        SC_FREE(utf8_string);
        sc_free_chunk_result(&worker->decoded);
        return 0;
    }
    if (fseeko(fp, (off_t) worker->checkpoint.file_offset, SEEK_SET) != 0) {
        fclose(fp);
        if (converter && converter != (iconv_t) -1) iconv_close(converter);
        SC_FREE(row_buffer);
        SC_FREE(raw_string);
        SC_FREE(utf8_string);
        sc_free_chunk_result(&worker->decoded);
        return 0;
    }

    state.missing_value = worker->info.missing_double;
    state.bias = worker->info.bias;
    state.bswap = worker->info.bswap;
    memcpy(state.chunk, worker->checkpoint.chunk, sizeof(state.chunk));
    state.i = worker->checkpoint.state_i;
    state.status = SAV_ROW_STREAM_NEED_DATA;

    while (row_index < worker->row_count) {
        if (buffer_used == 0 || state.status == SAV_ROW_STREAM_NEED_DATA) {
            if (!sc_sav_read_more(fp, input_buffer, sizeof(input_buffer), &buffer_used, &buffer_base_offset)) {
                fclose(fp);
                if (converter && converter != (iconv_t) -1) iconv_close(converter);
                SC_FREE(row_buffer);
                SC_FREE(raw_string);
                SC_FREE(utf8_string);
                sc_free_chunk_result(&worker->decoded);
                return 0;
            }
            state.next_in = input_buffer;
            state.avail_in = buffer_used;
            state.status = SAV_ROW_STREAM_HAVE_DATA;
        }

        state.next_out = row_buffer + row_out_offset;
        state.avail_out = worker->info.row_width - row_out_offset;
        sav_decompress_row(&state);
        buffer_used = state.avail_in;
        row_out_offset = worker->info.row_width - state.avail_out;

        if (state.status == SAV_ROW_STREAM_FINISHED_ROW) {
            double fp_value;
            int offset = 0;
            readstat_off_t data_offset = 0;
            size_t raw_str_used = 0;
            int segment_offset = 0;
            int var_index = 0;
            int col = 0;
            int raw_str_is_utf8 = ctx->input_encoding && !strcmp(ctx->input_encoding, "UTF-8");

            while (data_offset < (readstat_off_t) worker->info.row_width && col < ctx->var_index && var_index < ctx->var_index) {
                spss_varinfo_t *col_info = ctx->varinfo[col];
                spss_varinfo_t *var_info = ctx->varinfo[var_index];
                readstat_variable_t *variable = ctx->variables[var_info->index];
                int kept_index = variable->index_after_skipping;
                SCChunkColumn *out_col = &worker->decoded.cols[kept_index];
                readstat_value_t value;
                memset(&value, 0, sizeof(value));
                value.type = var_info->type;

                if (var_info->type == READSTAT_TYPE_STRING) {
                    size_t read_len = 8 - (offset == 31);
                    if (raw_str_used + read_len <= raw_string_len) {
                        if (raw_str_is_utf8) {
                            char c;
                            size_t k;
                            for (k = 0; k < read_len; ++k) {
                                if ((c = (char) row_buffer[data_offset + (readstat_off_t) k])) {
                                    raw_string[raw_str_used++] = c;
                                }
                            }
                        } else {
                            memcpy(raw_string + raw_str_used, &row_buffer[data_offset], read_len);
                            raw_str_used += read_len;
                        }
                    }
                    if (++offset == col_info->width) {
                        offset = 0;
                        col++;
                        segment_offset++;
                    }
                    if (segment_offset == var_info->n_segments) {
                        if (readstat_convert(utf8_string, utf8_string_len, raw_string, raw_str_used, converter) != READSTAT_OK) {
                            fclose(fp);
                            if (converter != (iconv_t) 0 && converter != (iconv_t) -1) {
                                iconv_close(converter);
                            }
                            SC_FREE(row_buffer);
                            SC_FREE(raw_string);
                            SC_FREE(utf8_string);
                            sc_free_chunk_result(&worker->decoded);
                            return 0;
                        }
                        value.v.string_value = utf8_string;

                        if ((!worker->user_na && readstat_value_is_defined_missing(value, variable)) || readstat_value_is_system_missing(value)) {
                            out_col->smissing[row_index] = 1;
                        } else {
                            out_col->svalues[row_index] = sc_strdup(value.v.string_value);
                            if (!out_col->svalues[row_index]) goto decode_done;
                        }
                        raw_str_used = 0;
                        segment_offset = 0;
                        var_index += var_info->n_segments;
                    }
                } else {
                    memcpy(&fp_value, &row_buffer[data_offset], 8);
                    if (ctx->bswap) {
                        fp_value = byteswap_double(fp_value);
                    }
                    value.v.double_value = fp_value;
                    sc_sav_tag_missing_double(&value, ctx);
                    if ((!worker->user_na && readstat_value_is_defined_missing(value, variable)) || readstat_value_is_system_missing(value)) {
                        out_col->dvalues[row_index] = NA_REAL;
                    } else if (readstat_value_is_defined_missing(value, variable)) {
                        out_col->dvalues[row_index] = fp_value;
                    } else {
                        out_col->dvalues[row_index] = adjustDatetimeToR(
                            DECLARED_SPSS,
                            worker->schema->cols[kept_index].var_type,
                            fp_value
                        );
                    }
                    var_index += var_info->n_segments;
                    col++;
                }
                data_offset += 8;
            }

            row_index++;
            row_out_offset = 0;
        } else if (state.status == SAV_ROW_STREAM_FINISHED_ALL) {
            break;
        }
    }

decode_done:
    fclose(fp);
    if (converter && converter != (iconv_t) -1) {
        iconv_close(converter);
    }
    SC_FREE(row_buffer);
    SC_FREE(raw_string);
    SC_FREE(utf8_string);
    worker->actual_rows = row_index;
    return row_index == worker->row_count;
}

#ifndef _WIN32
static void *sc_sav_rows_worker_thread_main(void *arg) {
    SCSavWorkerState *worker = (SCSavWorkerState *) arg;
    worker->error = !sc_sav_decode_chunk_rows(worker);
    return NULL;
}

static void *sc_sav_decode_worker_thread_main(void *arg) {
    SCSavWorkerState *worker = (SCSavWorkerState *) arg;
    worker->error = !sc_sav_decode_chunk_to_columns(worker);
    return NULL;
}
#endif

static void sc_sav_tag_missing_double(readstat_value_t *value, const sav_ctx_t *ctx) {
    double fp_value = value->v.double_value;
    uint64_t long_value = 0;
    memcpy(&long_value, &fp_value, 8);
    if (long_value == ctx->missing_double || long_value == ctx->lowest_double || long_value == ctx->highest_double || isnan(fp_value)) {
        value->is_system_missing = 1;
    }
}

SEXP declared_sav_parallel_prototype(SEXP spec, SEXP num_threads) {
    const char *path;
    ddiwr_sav_row_stream_info_t info;
    SCImportPlan plan;
    SCChunk *chunks = NULL;
    SCSavCheckpoint *checkpoints = NULL;
    SCSavWorkerState *workers = NULL;
    int nchunks = 0;
    int i;
    unsigned long long hash = 1469598103934665603ULL;
    char hash_buf[32];
    SEXP out;
    SEXP names;

    if (!sc_spec_path_cstr(spec, &path)) {
        Rf_error("Invalid file spec.");
    }
    if (sav_row_stream_info(path, NULL, &info) != READSTAT_OK) {
        Rf_error("Failed to prepare SAV stream.");
    }
    if (info.compression != READSTAT_COMPRESS_ROWS) {
        Rf_error("This prototype currently supports only row-compressed SAV files.");
    }
    if (info.record_count <= 0 || info.row_width == 0) {
        Rf_error("Invalid SAV stream metadata.");
    }

    memset(&plan, 0, sizeof(plan));
    plan.ext = DECLARED_SAV;
    plan.total_rows = info.record_count;
    plan.window_start = 0;
    plan.window_rows = info.record_count;
    plan.n_workers = sc_choose_worker_count(&plan, Rf_asInteger(num_threads));

    if (plan.n_workers <= 1 || !sc_plan_row_chunks(&plan, &chunks, &nchunks)) {
        plan.n_workers = 1;
        nchunks = 1;
        chunks = (SCChunk *) SC_CALLOC(1, SCChunk);
        chunks[0].start_row = 0;
        chunks[0].row_count = info.record_count;
    }

    checkpoints = (SCSavCheckpoint *) SC_CALLOC((size_t) nchunks, SCSavCheckpoint);
    if (!sc_sav_collect_checkpoints(path, &info, chunks, nchunks, checkpoints)) {
        SC_FREE(checkpoints);
        SC_FREE(chunks);
        Rf_error("Failed to collect SAV chunk checkpoints.");
    }

    workers = (SCSavWorkerState *) SC_CALLOC((size_t) nchunks, SCSavWorkerState);
    for (i = 0; i < nchunks; ++i) {
        memset(&workers[i], 0, sizeof(SCSavWorkerState));
        workers[i].path = path;
        workers[i].info = info;
        workers[i].checkpoint = checkpoints[i];
        workers[i].row_count = chunks[i].row_count;
    }

#ifdef _WIN32
    for (i = 0; i < nchunks; ++i) {
        workers[i].error = !sc_sav_decode_chunk_rows(&workers[i]);
    }
#else
    for (i = 0; i < nchunks; ++i) {
        if (pthread_create(&workers[i].thread, NULL, sc_sav_rows_worker_thread_main, &workers[i]) != 0) {
            workers[i].error = 1;
            break;
        }
        workers[i].thread_started = 1;
    }
    for (i = 0; i < nchunks; ++i) {
        if (workers[i].thread_started) {
            pthread_join(workers[i].thread, NULL);
        }
    }
#endif

    for (i = 0; i < nchunks; ++i) {
        if (workers[i].error || workers[i].result.rows == NULL) {
            int j;
            long expected_rows = workers[i].row_count;
            long actual_rows = workers[i].actual_rows;
            for (j = 0; j < nchunks; ++j) {
                if (workers[j].result.rows != NULL) {
                    SC_FREE(workers[j].result.rows);
                }
            }
            SC_FREE(workers);
            SC_FREE(checkpoints);
            SC_FREE(chunks);
            Rf_error("Failed to decode SAV chunk %d in parallel (expected %ld rows, got %ld).",
                     i + 1, expected_rows, actual_rows);
        }
        hash = sc_fnv1a64_update(
            hash,
            workers[i].result.rows,
            (size_t) workers[i].result.nrows * workers[i].result.row_width
        );
    }

    snprintf(hash_buf, sizeof(hash_buf), "%016llx", (unsigned long long) hash);

    PROTECT(out = Rf_allocVector(VECSXP, 6));
    PROTECT(names = Rf_allocVector(STRSXP, 6));
    SET_STRING_ELT(names, 0, Rf_mkChar("rows"));
    SET_STRING_ELT(names, 1, Rf_mkChar("row_width"));
    SET_STRING_ELT(names, 2, Rf_mkChar("workers"));
    SET_STRING_ELT(names, 3, Rf_mkChar("compression"));
    SET_STRING_ELT(names, 4, Rf_mkChar("checksum"));
    SET_STRING_ELT(names, 5, Rf_mkChar("chunk_rows"));
    SET_VECTOR_ELT(out, 0, Rf_ScalarReal((double) info.record_count));
    SET_VECTOR_ELT(out, 1, Rf_ScalarReal((double) info.row_width));
    SET_VECTOR_ELT(out, 2, Rf_ScalarInteger(nchunks));
    SET_VECTOR_ELT(out, 3, Rf_ScalarInteger((int) info.compression));
    SET_VECTOR_ELT(out, 4, Rf_mkString(hash_buf));
    {
        SEXP chunk_rows = PROTECT(Rf_allocVector(REALSXP, nchunks));
        for (i = 0; i < nchunks; ++i) {
            REAL(chunk_rows)[i] = (double) chunks[i].row_count;
        }
        SET_VECTOR_ELT(out, 5, chunk_rows);
        UNPROTECT(1);
    }
    Rf_setAttrib(out, R_NamesSymbol, names);
    UNPROTECT(2);

    for (i = 0; i < nchunks; ++i) {
        if (workers[i].result.rows != NULL) {
            SC_FREE(workers[i].result.rows);
        }
    }
    SC_FREE(workers);
    SC_FREE(checkpoints);
    SC_FREE(chunks);
    return out;
}

SEXP declared_df_parse_sav_file_parallel_prototype(SEXP spec, SEXP encoding, SEXP user_na, SEXP num_threads) {
    const char *path;
    const char *encoding_c = NULL;
    ddiwr_sav_row_stream_info_t info;
    ddiwr_sav_prepared_t prepared;
    SCSchema schema;
    SCImportPlan plan;
    SCChunk *chunks = NULL;
    SCSavCheckpoint *checkpoints = NULL;
    SCSavWorkerState *workers = NULL;
    int nchunks = 0;
    int i;

    if (!sc_spec_path_cstr(spec, &path)) {
        Rf_error("Invalid file spec.");
    }
    if (!Rf_isNull(encoding)) {
        encoding_c = CHAR(STRING_ELT(encoding, 0));
    }

    memset(&prepared, 0, sizeof(prepared));
    memset(&schema, 0, sizeof(schema));
    memset(&plan, 0, sizeof(plan));

    if (sc_probe_readstat_metadata(DECLARED_SAV, path, encoding_c, NULL, 0, &schema) != READSTAT_OK) {
        Rf_error("Failed to probe SAV schema.");
    }
    if (sav_row_stream_info(path, encoding_c, &info) != READSTAT_OK) {
        sc_free_schema(&schema);
        Rf_error("Failed to probe SAV stream.");
    }
    if (sav_prepare(path, encoding_c, &prepared) != READSTAT_OK) {
        sc_free_schema(&schema);
        Rf_error("Failed to prepare SAV decoder.");
    }
    if (info.compression != READSTAT_COMPRESS_ROWS) {
        sav_prepared_free(&prepared);
        sc_free_schema(&schema);
        Rf_error("This prototype currently supports only row-compressed SAV files.");
    }

    plan.ext = DECLARED_SAV;
    plan.total_rows = info.record_count;
    plan.window_start = 0;
    plan.window_rows = info.record_count;
    plan.n_workers = sc_choose_worker_count(&plan, Rf_asInteger(num_threads));

    if (plan.n_workers <= 1 || !sc_plan_row_chunks(&plan, &chunks, &nchunks)) {
        plan.n_workers = 1;
        nchunks = 1;
        chunks = (SCChunk *) SC_CALLOC(1, SCChunk);
        chunks[0].start_row = 0;
        chunks[0].row_count = info.record_count;
    }

    checkpoints = (SCSavCheckpoint *) SC_CALLOC((size_t) nchunks, SCSavCheckpoint);
    if (!sc_sav_collect_checkpoints(path, &info, chunks, nchunks, checkpoints)) {
        sav_prepared_free(&prepared);
        sc_free_schema(&schema);
        SC_FREE(checkpoints);
        SC_FREE(chunks);
        Rf_error("Failed to collect SAV chunk checkpoints.");
    }

    workers = (SCSavWorkerState *) SC_CALLOC((size_t) nchunks, SCSavWorkerState);
    for (i = 0; i < nchunks; ++i) {
        memset(&workers[i], 0, sizeof(SCSavWorkerState));
        workers[i].path = path;
        workers[i].info = info;
        workers[i].checkpoint = checkpoints[i];
        workers[i].row_count = chunks[i].row_count;
        workers[i].schema = &schema;
        workers[i].sav_ctx = prepared.ctx;
        workers[i].input_encoding = prepared.ctx->input_encoding;
        workers[i].user_na = Rf_asLogical(user_na);
    }

#ifdef _WIN32
    for (i = 0; i < nchunks; ++i) {
        workers[i].error = !sc_sav_decode_chunk_to_columns(&workers[i]);
    }
#else
    for (i = 0; i < nchunks; ++i) {
        if (pthread_create(&workers[i].thread, NULL, sc_sav_decode_worker_thread_main, &workers[i]) != 0) {
            workers[i].error = 1;
            break;
        }
        workers[i].thread_started = 1;
    }
    for (i = 0; i < nchunks; ++i) {
        if (workers[i].thread_started) {
            pthread_join(workers[i].thread, NULL);
        }
    }
#endif

    for (i = 0; i < nchunks; ++i) {
        if (workers[i].error || workers[i].decoded.cols == NULL) {
            int j;
            for (j = 0; j < nchunks; ++j) {
                sc_free_chunk_result(&workers[j].decoded);
            }
            SC_FREE(workers);
            SC_FREE(checkpoints);
            SC_FREE(chunks);
            sav_prepared_free(&prepared);
            sc_free_schema(&schema);
            Rf_error("Failed to decode SAV chunk in full prototype.");
        }
    }

    for (i = 0; i < nchunks; i++) chunks[i].decoded = &workers[i].decoded;
    SC_FREE(checkpoints);
    sav_prepared_free(&prepared);
    SCOutput output = {&schema, chunks, nchunks, Rf_asLogical(user_na), workers};
    return R_ExecWithCleanup(sc_build_r_output, &output, sc_output_cleanup, &output);
}
