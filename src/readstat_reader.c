#include <R.h>
#include <Rinternals.h>
#include <R_ext/RS.h>
#include <R_ext/Utils.h>

#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "readstat.h"
#include "declared_types.h"
#include "tagged_na.h"

typedef enum {
    LABELSET_NONE = 0,
    LABELSET_STRING = 1,
    LABELSET_DOUBLE = 2
} LabelSetType;

typedef struct {
    char *name;
    LabelSetType type;
    size_t n;
    size_t cap;
    char **labels;
    char **svalues;
    double *dvalues;
} LabelSet;

typedef struct {
    SEXP vector;
    char *val_labels_name;
    VarType var_type;
    int readstat_type;
    int has_tagged_missing;
    size_t na_index_n;
    size_t na_index_cap;
    int *na_index_pos;
    char **na_index_names;
} ColumnInfo;

typedef struct {
    FileExt ext;
    FileVendor vendor;
    int user_na;
    long nrows;
    long nalloc;
    int ncols;
    int protect_count;
    int *cols_skip;
    int cols_skip_n;
    SEXP output;
    SEXP names;
    LabelSet *label_sets;
    size_t label_set_n;
    size_t label_set_cap;
    ColumnInfo *cols;
    char **notes;
    size_t notes_n;
    size_t notes_cap;
} ReaderCtx;

static char *str_dup(const char *x) {
    size_t n;
    char *out;
    if (x == NULL) {
        return NULL;
    }
    n = strlen(x);
    out = (char *) R_Calloc(n + 1, char);
    memcpy(out, x, n + 1);
    return out;
}

static int is_intish(double x) {
    double ix = (double) ((int) x);
    return !ISNAN(x) && x == ix;
}

static double tag_code_value(char tag) {
    char lower = (char) tolower((unsigned char) tag);
    if (lower < 'a' || lower > 'z') {
        return NA_REAL;
    }
    return -91.0 - (double) (lower - 'a');
}

static SEXP tag_code_name(char tag) {
    char buffer[32];
    double code = tag_code_value(tag);
    if (ISNAN(code)) {
        return NA_STRING;
    }
    snprintf(buffer, sizeof(buffer), "%.15g", code);
    return Rf_mkCharCE(buffer, CE_UTF8);
}

static int cols_skip_contains(ReaderCtx *ctx, int index) {
    int i;
    for (i = 0; i < ctx->cols_skip_n; ++i) {
        if (ctx->cols_skip[i] == index) {
            return 1;
        }
    }
    return 0;
}

static void ensure_notes_capacity(ReaderCtx *ctx) {
    if (ctx->notes_n >= ctx->notes_cap) {
        ctx->notes_cap = ctx->notes_cap == 0 ? 4 : ctx->notes_cap * 2;
        ctx->notes = (char **) R_Realloc(ctx->notes, ctx->notes_cap, char *);
    }
}

static void ensure_labelset_capacity(LabelSet *set) {
    if (set->n >= set->cap) {
        set->cap = set->cap == 0 ? 8 : set->cap * 2;
        set->labels = (char **) R_Realloc(set->labels, set->cap, char *);
        if (set->type == LABELSET_STRING) {
            set->svalues = (char **) R_Realloc(set->svalues, set->cap, char *);
        } else if (set->type == LABELSET_DOUBLE) {
            set->dvalues = (double *) R_Realloc(set->dvalues, set->cap, double);
        }
    }
}

static LabelSet *find_or_create_labelset(ReaderCtx *ctx, const char *name, LabelSetType type) {
    size_t i;
    LabelSet *set;

    for (i = 0; i < ctx->label_set_n; ++i) {
        if (strcmp(ctx->label_sets[i].name, name) == 0) {
            return &ctx->label_sets[i];
        }
    }

    if (ctx->label_set_n >= ctx->label_set_cap) {
        ctx->label_set_cap = ctx->label_set_cap == 0 ? 8 : ctx->label_set_cap * 2;
        ctx->label_sets = (LabelSet *) R_Realloc(ctx->label_sets, ctx->label_set_cap, LabelSet);
    }

    set = &ctx->label_sets[ctx->label_set_n++];
    memset(set, 0, sizeof(*set));
    set->name = str_dup(name);
    set->type = type;
    return set;
}

static LabelSet *find_labelset(ReaderCtx *ctx, const char *name) {
    size_t i;
    if (name == NULL || *name == '\0') {
        return NULL;
    }
    for (i = 0; i < ctx->label_set_n; ++i) {
        if (strcmp(ctx->label_sets[i].name, name) == 0) {
            return &ctx->label_sets[i];
        }
    }
    return NULL;
}

static void add_na_index(ColumnInfo *col, int pos, char tag) {
    char *tag_name;
    if (col->na_index_n >= col->na_index_cap) {
        col->na_index_cap = col->na_index_cap == 0 ? 8 : col->na_index_cap * 2;
        col->na_index_pos = (int *) R_Realloc(col->na_index_pos, col->na_index_cap, int);
        col->na_index_names = (char **) R_Realloc(col->na_index_names, col->na_index_cap, char *);
    }
    tag_name = (char *) R_Calloc(2, char);
    tag_name[0] = (char) tolower((unsigned char) tag);
    tag_name[1] = '\0';
    col->na_index_pos[col->na_index_n] = pos;
    col->na_index_names[col->na_index_n] = tag_name;
    col->na_index_n++;
    col->has_tagged_missing = 1;
}

static SEXP spec_path(SEXP spec) {
    if (!Rf_isNewList(spec) || Rf_length(spec) < 1) {
        Rf_error("Invalid file spec.");
    }
    return VECTOR_ELT(spec, 0);
}

static const char *spec_path_cstr(SEXP spec) {
    SEXP path = spec_path(spec);
    if (!Rf_isString(path) || Rf_length(path) != 1) {
        Rf_error("Invalid file path.");
    }
    return CHAR(STRING_ELT(path, 0));
}

static void reader_ctx_init(ReaderCtx *ctx, FileExt ext, int user_na, SEXP cols_skip) {
    int i;
    memset(ctx, 0, sizeof(*ctx));
    ctx->ext = ext;
    ctx->vendor = extVendor(ext);
    ctx->user_na = user_na;
    ctx->nrows = 0;
    ctx->nalloc = 0;
    ctx->ncols = 0;
    ctx->protect_count = 0;
    if (Rf_isInteger(cols_skip) || Rf_isReal(cols_skip)) {
        ctx->cols_skip_n = Rf_length(cols_skip);
        if (ctx->cols_skip_n > 0) {
            ctx->cols_skip = (int *) R_Calloc(ctx->cols_skip_n, int);
            for (i = 0; i < ctx->cols_skip_n; ++i) {
                ctx->cols_skip[i] = TYPEOF(cols_skip) == INTSXP ?
                    INTEGER(cols_skip)[i] :
                    (int) REAL(cols_skip)[i];
            }
        }
    }
}

static void reader_ctx_free(ReaderCtx *ctx) {
    size_t i;
    size_t j;
    if (ctx->cols_skip != NULL) {
        R_Free(ctx->cols_skip);
    }
    if (ctx->cols != NULL) {
        for (i = 0; i < (size_t) ctx->ncols; ++i) {
            if (ctx->cols[i].val_labels_name != NULL) {
                R_Free(ctx->cols[i].val_labels_name);
            }
            for (j = 0; j < ctx->cols[i].na_index_n; ++j) {
                R_Free(ctx->cols[i].na_index_names[j]);
            }
            if (ctx->cols[i].na_index_pos != NULL) {
                R_Free(ctx->cols[i].na_index_pos);
            }
            if (ctx->cols[i].na_index_names != NULL) {
                R_Free(ctx->cols[i].na_index_names);
            }
        }
        R_Free(ctx->cols);
    }
    for (i = 0; i < ctx->label_set_n; ++i) {
        if (ctx->label_sets[i].name != NULL) {
            R_Free(ctx->label_sets[i].name);
        }
        for (j = 0; j < ctx->label_sets[i].n; ++j) {
            R_Free(ctx->label_sets[i].labels[j]);
            if (ctx->label_sets[i].type == LABELSET_STRING) {
                R_Free(ctx->label_sets[i].svalues[j]);
            }
        }
        if (ctx->label_sets[i].labels != NULL) {
            R_Free(ctx->label_sets[i].labels);
        }
        if (ctx->label_sets[i].svalues != NULL) {
            R_Free(ctx->label_sets[i].svalues);
        }
        if (ctx->label_sets[i].dvalues != NULL) {
            R_Free(ctx->label_sets[i].dvalues);
        }
    }
    if (ctx->label_sets != NULL) {
        R_Free(ctx->label_sets);
    }
    for (i = 0; i < ctx->notes_n; ++i) {
        R_Free(ctx->notes[i]);
    }
    if (ctx->notes != NULL) {
        R_Free(ctx->notes);
    }
}

static void resize_columns(ReaderCtx *ctx, long new_n) {
    int i;
    for (i = 0; i < ctx->ncols; ++i) {
        SEXP col = VECTOR_ELT(ctx->output, i);
        SEXP new_col = PROTECT(Rf_lengthgets(col, new_n));
        Rf_copyMostAttrib(col, new_col);
        SET_VECTOR_ELT(ctx->output, i, new_col);
        ctx->cols[i].vector = new_col;
        UNPROTECT(1);
    }
    ctx->nalloc = new_n;
}

static void ensure_row_capacity(ReaderCtx *ctx, long row) {
    long new_n;
    if (row < ctx->nalloc) {
        return;
    }
    new_n = ctx->nalloc > 0 ? ctx->nalloc : 100000;
    while (row >= new_n) {
        new_n *= 2;
    }
    resize_columns(ctx, new_n);
}

static void set_vector_class(SEXP col, VarType var_type) {
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

static int reader_metadata(readstat_metadata_t *metadata, void *ctx_) {
    ReaderCtx *ctx = (ReaderCtx *) ctx_;
    int var_count = readstat_get_var_count(metadata);
    int kept = var_count - ctx->cols_skip_n;

    if (kept < 0) {
        kept = 0;
    }

    ctx->nalloc = readstat_get_row_count(metadata);
    if (ctx->nalloc < 0) {
        ctx->nalloc = 100000;
        ctx->nrows = 0;
    } else {
        ctx->nrows = ctx->nalloc;
    }

    ctx->ncols = kept;
    PROTECT(ctx->output = Rf_allocVector(VECSXP, kept));
    PROTECT(ctx->names = Rf_allocVector(STRSXP, kept));
    ctx->protect_count += 2;
    ctx->cols = (ColumnInfo *) R_Calloc((size_t) kept, ColumnInfo);

    if (readstat_get_file_label(metadata) != NULL && strcmp(readstat_get_file_label(metadata), "") != 0) {
        Rf_setAttrib(ctx->output, Rf_install("label"), Rf_mkString(readstat_get_file_label(metadata)));
    }

    return READSTAT_HANDLER_OK;
}

static int reader_note(int note_index, const char *note, void *ctx_) {
    ReaderCtx *ctx = (ReaderCtx *) ctx_;
    (void) note_index;
    if (note == NULL || strcmp(note, "") == 0) {
        return READSTAT_HANDLER_OK;
    }
    ensure_notes_capacity(ctx);
    ctx->notes[ctx->notes_n++] = str_dup(note);
    return READSTAT_HANDLER_OK;
}

static int reader_variable(int index, readstat_variable_t *variable, const char *val_labels, void *ctx_) {
    ReaderCtx *ctx = (ReaderCtx *) ctx_;
    int var_index;
    SEXP col;
    const char *name;
    const char *var_label;
    const char *var_format;
    int n_ranges;
    int i;

    if (cols_skip_contains(ctx, index)) {
        return READSTAT_HANDLER_SKIP_VARIABLE;
    }

    var_index = readstat_variable_get_index_after_skipping(variable);
    name = readstat_variable_get_name(variable);
    SET_STRING_ELT(ctx->names, var_index, Rf_mkCharCE(name == NULL ? "" : name, CE_UTF8));

    switch (readstat_variable_get_type(variable)) {
        case READSTAT_TYPE_STRING_REF:
        case READSTAT_TYPE_STRING:
            PROTECT(col = Rf_allocVector(STRSXP, ctx->nalloc));
            break;
        default:
            PROTECT(col = Rf_allocVector(REALSXP, ctx->nalloc));
            for (i = 0; i < ctx->nalloc; ++i) {
                REAL(col)[i] = NA_REAL;
            }
            break;
    }
    ctx->protect_count++;

    ctx->cols[var_index].vector = col;
    ctx->cols[var_index].readstat_type = readstat_variable_get_type(variable);
    ctx->cols[var_index].var_type = numTypeFromFormat(ctx->vendor, readstat_variable_get_format(variable));
    if (val_labels != NULL) {
        ctx->cols[var_index].val_labels_name = str_dup(val_labels);
    }

    var_label = readstat_variable_get_label(variable);
    if (var_label != NULL && strcmp(var_label, "") != 0) {
        Rf_setAttrib(col, Rf_install("label"), Rf_mkString(var_label));
    }

    var_format = readstat_variable_get_format(variable);
    if (var_format != NULL && strcmp(var_format, "") != 0) {
        Rf_setAttrib(col, Rf_install(formatAttribute(ctx->vendor)), Rf_mkString(var_format));
    }

    if (ctx->vendor == DECLARED_SPSS && readstat_variable_get_display_width(variable) != 8) {
        Rf_setAttrib(col, Rf_install("display_width"), Rf_ScalarInteger(readstat_variable_get_display_width(variable)));
    }

    if (ctx->cols[var_index].var_type != DECLARED_DEFAULT &&
        readstat_variable_get_type_class(variable) == READSTAT_TYPE_CLASS_STRING) {
        Rf_warning("String variable '%s' has incompatible format '%s' and will be returned as a regular string variable.",
                   name == NULL ? "" : name,
                   var_format == NULL ? "" : var_format);
    } else {
        set_vector_class(col, ctx->cols[var_index].var_type);
    }

    n_ranges = readstat_variable_get_missing_ranges_count(variable);
    if (ctx->user_na && n_ranges > 0) {
        if (readstat_variable_get_type(variable) == READSTAT_TYPE_STRING ||
            readstat_variable_get_type(variable) == READSTAT_TYPE_STRING_REF) {
            SEXP na_values = PROTECT(Rf_allocVector(STRSXP, 0));
            SEXP na_range = PROTECT(Rf_allocVector(STRSXP, 2));
            int has_range = 0;
            int single_count = 0;

            for (i = 0; i < n_ranges; ++i) {
                readstat_value_t lo_value = readstat_variable_get_missing_range_lo(variable, i);
                readstat_value_t hi_value = readstat_variable_get_missing_range_hi(variable, i);
                const char *lo = readstat_string_value(lo_value);
                const char *hi = readstat_string_value(hi_value);
                if (lo == hi) {
                    na_values = Rf_lengthgets(na_values, single_count + 1);
                    SET_STRING_ELT(na_values, single_count, lo == NULL ? NA_STRING : Rf_mkCharCE(lo, CE_UTF8));
                    single_count++;
                } else {
                    has_range = 1;
                    SET_STRING_ELT(na_range, 0, lo == NULL ? NA_STRING : Rf_mkCharCE(lo, CE_UTF8));
                    SET_STRING_ELT(na_range, 1, hi == NULL ? NA_STRING : Rf_mkCharCE(hi, CE_UTF8));
                }
            }
            if (Rf_length(na_values) > 0) {
                Rf_setAttrib(col, Rf_install("na_values"), na_values);
            }
            if (has_range) {
                Rf_setAttrib(col, Rf_install("na_range"), na_range);
            }
            UNPROTECT(2);
        } else {
            int single_count = 0;
            int has_range = 0;
            SEXP na_values = PROTECT(Rf_allocVector(REALSXP, 0));
            SEXP na_range = PROTECT(Rf_allocVector(REALSXP, 2));

            for (i = 0; i < n_ranges; ++i) {
                readstat_value_t lo_value = readstat_variable_get_missing_range_lo(variable, i);
                readstat_value_t hi_value = readstat_variable_get_missing_range_hi(variable, i);
                double lo = readstat_double_value(lo_value);
                double hi = readstat_double_value(hi_value);
                if (lo == hi) {
                    na_values = Rf_lengthgets(na_values, single_count + 1);
                    REAL(na_values)[single_count] = lo;
                    single_count++;
                } else {
                    has_range = 1;
                    REAL(na_range)[0] = lo;
                    REAL(na_range)[1] = hi;
                }
            }
            if (Rf_length(na_values) > 0) {
                Rf_setAttrib(col, Rf_install("na_values"), na_values);
            }
            if (has_range) {
                Rf_setAttrib(col, Rf_install("na_range"), na_range);
            }
            UNPROTECT(2);
        }
    }

    SET_VECTOR_ELT(ctx->output, var_index, col);
    UNPROTECT(1);
    ctx->protect_count--;
    return READSTAT_HANDLER_OK;
}

static int reader_value(int obs_index, readstat_variable_t *variable, readstat_value_t value, void *ctx_) {
    ReaderCtx *ctx = (ReaderCtx *) ctx_;
    int var_index = readstat_variable_get_index_after_skipping(variable);
    ColumnInfo *col_info = &ctx->cols[var_index];
    SEXP col = VECTOR_ELT(ctx->output, var_index);

    ensure_row_capacity(ctx, obs_index);
    if (obs_index >= ctx->nrows) {
        ctx->nrows = obs_index + 1;
    }

    switch (value.type) {
        case READSTAT_TYPE_STRING_REF:
        case READSTAT_TYPE_STRING: {
            const char *str_value = readstat_string_value(value);
            if (readstat_value_is_tagged_missing(value) ||
                (!ctx->user_na && readstat_value_is_defined_missing(value, variable)) ||
                readstat_value_is_system_missing(value)) {
                SET_STRING_ELT(col, obs_index, NA_STRING);
            } else if (str_value == NULL) {
                SET_STRING_ELT(col, obs_index, Rf_mkCharCE("", CE_UTF8));
            } else {
                SET_STRING_ELT(col, obs_index, Rf_mkCharCE(str_value, CE_UTF8));
            }
            break;
        }
        default: {
            if (readstat_value_is_tagged_missing(value)) {
                REAL(col)[obs_index] = NA_REAL;
                add_na_index(col_info, obs_index + 1, readstat_value_tag(value));
            } else if ((!ctx->user_na && readstat_value_is_defined_missing(value, variable)) ||
                       readstat_value_is_system_missing(value)) {
                REAL(col)[obs_index] = NA_REAL;
            } else {
                REAL(col)[obs_index] = adjustDatetimeToR(
                    ctx->vendor,
                    col_info->var_type,
                    readstat_double_value(value)
                );
            }
            break;
        }
    }
    return READSTAT_HANDLER_OK;
}

static int reader_value_label(const char *val_labels, readstat_value_t value, const char *label, void *ctx_) {
    ReaderCtx *ctx = (ReaderCtx *) ctx_;
    LabelSet *set;
    if (val_labels == NULL || label == NULL) {
        return READSTAT_HANDLER_OK;
    }

    switch (value.type) {
        case READSTAT_TYPE_STRING:
            set = find_or_create_labelset(ctx, val_labels, LABELSET_STRING);
            ensure_labelset_capacity(set);
            set->labels[set->n] = str_dup(label);
            set->svalues[set->n] = str_dup(readstat_string_value(value));
            set->n++;
            break;
        default:
            set = find_or_create_labelset(ctx, val_labels, LABELSET_DOUBLE);
            ensure_labelset_capacity(set);
            set->labels[set->n] = str_dup(label);
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

static SEXP make_names_from_labels(LabelSet *set) {
    size_t i;
    SEXP names = PROTECT(Rf_allocVector(STRSXP, (R_xlen_t) set->n));
    for (i = 0; i < set->n; ++i) {
        SET_STRING_ELT(names, i, Rf_mkCharCE(set->labels[i], CE_UTF8));
    }
    UNPROTECT(1);
    return names;
}

static SEXP labelset_to_sexp(LabelSet *set) {
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
            REAL(out)[i] = tag_code_value(tag);
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

static void finalize_tagged_missing_attrs(ColumnInfo *col_info, SEXP col) {
    size_t i;
    SEXP na_index;
    SEXP na_names;
    SEXP na_values;
    int seen[26];
    int unique_n = 0;

    if (col_info->na_index_n == 0) {
        return;
    }

    PROTECT(na_index = Rf_allocVector(INTSXP, (R_xlen_t) col_info->na_index_n));
    PROTECT(na_names = Rf_allocVector(STRSXP, (R_xlen_t) col_info->na_index_n));
    memset(seen, 0, sizeof(seen));
    for (i = 0; i < col_info->na_index_n; ++i) {
        char tag = col_info->na_index_names[i][0];
        char lower = (char) tolower((unsigned char) tag);
        INTEGER(na_index)[i] = col_info->na_index_pos[i];
        SET_STRING_ELT(na_names, i, tag_code_name(tag));
        if (lower >= 'a' && lower <= 'z' && !seen[lower - 'a']) {
            seen[lower - 'a'] = 1;
            unique_n++;
        }
    }
    Rf_setAttrib(na_index, R_NamesSymbol, na_names);
    Rf_setAttrib(col, Rf_install("na_index"), na_index);

    PROTECT(na_values = Rf_allocVector(REALSXP, unique_n));
    unique_n = 0;
    memset(seen, 0, sizeof(seen));
    for (i = 0; i < col_info->na_index_n; ++i) {
        char tag = col_info->na_index_names[i][0];
        char lower = (char) tolower((unsigned char) tag);
        if (lower >= 'a' && lower <= 'z' && !seen[lower - 'a']) {
            REAL(na_values)[unique_n++] = tag_code_value(lower);
            seen[lower - 'a'] = 1;
        }
    }
    if (unique_n < Rf_length(na_values)) {
        SEXP shrunk = PROTECT(Rf_lengthgets(na_values, unique_n));
        Rf_setAttrib(col, Rf_install("na_values"), shrunk);
        UNPROTECT(1);
    } else {
        Rf_setAttrib(col, Rf_install("na_values"), na_values);
    }
    UNPROTECT(3);
}

static SEXP finalize_output(ReaderCtx *ctx) {
    int i;
    SEXP row_names;
    SEXP notes;

    if (ctx->nrows != ctx->nalloc) {
        resize_columns(ctx, ctx->nrows);
    }

    for (i = 0; i < ctx->ncols; ++i) {
        SEXP col = VECTOR_ELT(ctx->output, i);
        LabelSet *set = find_labelset(ctx, ctx->cols[i].val_labels_name);
        if (set != NULL) {
            SEXP labels = PROTECT(labelset_to_sexp(set));
            Rf_setAttrib(col, Rf_install("labels"), labels);
            UNPROTECT(1);
        }
        finalize_tagged_missing_attrs(&ctx->cols[i], col);
    }

    if (ctx->notes_n > 0) {
        PROTECT(notes = Rf_allocVector(STRSXP, (R_xlen_t) ctx->notes_n));
        for (i = 0; i < (int) ctx->notes_n; ++i) {
            SET_STRING_ELT(notes, i, Rf_mkCharCE(ctx->notes[i], CE_UTF8));
        }
        Rf_setAttrib(ctx->output, Rf_install("notes"), notes);
        UNPROTECT(1);
    }

    Rf_setAttrib(ctx->output, R_NamesSymbol, ctx->names);
    PROTECT(row_names = Rf_allocVector(INTSXP, 2));
    INTEGER(row_names)[0] = NA_INTEGER;
    INTEGER(row_names)[1] = -(int) ctx->nrows;
    Rf_setAttrib(ctx->output, R_RowNamesSymbol, row_names);
    Rf_setAttrib(ctx->output, R_ClassSymbol, Rf_mkString("data.frame"));
    UNPROTECT(1);

    UNPROTECT(ctx->protect_count);
    return ctx->output;
}

static readstat_parser_t *init_parser(void) {
    readstat_parser_t *parser = readstat_parser_init();
    readstat_set_metadata_handler(parser, reader_metadata);
    readstat_set_note_handler(parser, reader_note);
    readstat_set_variable_handler(parser, reader_variable);
    readstat_set_value_handler(parser, reader_value);
    readstat_set_value_label_handler(parser, reader_value_label);
    return parser;
}

static SEXP parse_file(FileExt ext, SEXP spec, SEXP encoding, SEXP user_na, SEXP cols_skip, SEXP n_max, SEXP rows_skip, SEXP spec_cat, SEXP catalog_encoding) {
    ReaderCtx ctx;
    readstat_parser_t *parser;
    readstat_error_t result = READSTAT_OK;
    const char *path;
    const char *cat_path = NULL;
    long nmax = -1;
    long skip = 0;
    const char *enc = NULL;
    const char *cat_enc = NULL;
    SEXP out;

    reader_ctx_init(&ctx, ext, Rf_asLogical(user_na), cols_skip);

    parser = init_parser();
    if (parser == NULL) {
        reader_ctx_free(&ctx);
        Rf_error("Failed to initialize ReadStat parser.");
    }

    if (!Rf_isNull(encoding)) {
        enc = CHAR(STRING_ELT(encoding, 0));
        if (enc != NULL && *enc != '\0') {
            readstat_set_file_character_encoding(parser, enc);
        }
    }

    if (!Rf_isNull(n_max)) {
        nmax = Rf_asInteger(n_max);
        readstat_set_row_limit(parser, nmax == 0 ? 1 : nmax);
    }
    if (!Rf_isNull(rows_skip)) {
        skip = Rf_asInteger(rows_skip);
        readstat_set_row_offset(parser, skip);
    }

    path = spec_path_cstr(spec);

    if (ext == DECLARED_SAS7BDAT && Rf_isNewList(spec_cat) && Rf_length(spec_cat) > 0) {
        cat_path = spec_path_cstr(spec_cat);
        if (!Rf_isNull(catalog_encoding)) {
            cat_enc = CHAR(STRING_ELT(catalog_encoding, 0));
            if (cat_enc != NULL && *cat_enc != '\0') {
                readstat_set_file_character_encoding(parser, cat_enc);
            }
        }
        result = readstat_parse_sas7bcat(parser, cat_path, &ctx);
        if (!Rf_isNull(encoding) && enc != NULL && *enc != '\0') {
            readstat_set_file_character_encoding(parser, enc);
        }
    }

    if (result == READSTAT_OK) {
        switch (ext) {
            case DECLARED_SAV:
                result = readstat_parse_sav(parser, path, &ctx);
                break;
            case DECLARED_POR:
                result = readstat_parse_por(parser, path, &ctx);
                break;
            case DECLARED_DTA:
                result = readstat_parse_dta(parser, path, &ctx);
                break;
            case DECLARED_SAS7BDAT:
                result = readstat_parse_sas7bdat(parser, path, &ctx);
                break;
            case DECLARED_XPT:
                result = readstat_parse_xport(parser, path, &ctx);
                break;
            default:
                result = READSTAT_ERROR_PARSE;
                break;
        }
    }

    readstat_parser_free(parser);
    if (result != READSTAT_OK) {
        reader_ctx_free(&ctx);
        Rf_error("Failed to parse %s: %s", path, readstat_error_message(result));
    }

    if (nmax == 0) {
        ctx.nrows = 0;
    } else if (nmax > 0 && ctx.nrows > nmax) {
        ctx.nrows = nmax;
    }

    out = finalize_output(&ctx);
    reader_ctx_free(&ctx);
    return out;
}

SEXP declared_df_parse_sav_file(SEXP spec, SEXP encoding, SEXP user_na, SEXP cols_skip, SEXP n_max, SEXP rows_skip) {
    return parse_file(DECLARED_SAV, spec, encoding, user_na, cols_skip, n_max, rows_skip, R_NilValue, R_NilValue);
}

SEXP declared_df_parse_por_file(SEXP spec, SEXP encoding, SEXP user_na, SEXP cols_skip, SEXP n_max, SEXP rows_skip) {
    return parse_file(DECLARED_POR, spec, encoding, user_na, cols_skip, n_max, rows_skip, R_NilValue, R_NilValue);
}

SEXP declared_df_parse_dta_file(SEXP spec, SEXP encoding, SEXP cols_skip, SEXP n_max, SEXP rows_skip) {
    return parse_file(DECLARED_DTA, spec, encoding, ScalarLogical(1), cols_skip, n_max, rows_skip, R_NilValue, R_NilValue);
}

SEXP declared_df_parse_sas_file(SEXP spec_b7dat, SEXP spec_b7cat, SEXP encoding, SEXP catalog_encoding, SEXP cols_skip, SEXP n_max, SEXP rows_skip) {
    return parse_file(DECLARED_SAS7BDAT, spec_b7dat, encoding, ScalarLogical(0), cols_skip, n_max, rows_skip, spec_b7cat, catalog_encoding);
}

SEXP declared_df_parse_xpt_file(SEXP spec, SEXP cols_skip, SEXP n_max, SEXP rows_skip) {
    return parse_file(DECLARED_XPT, spec, R_NilValue, ScalarLogical(0), cols_skip, n_max, rows_skip, R_NilValue, R_NilValue);
}
