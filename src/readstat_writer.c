#include <R.h>
#include <Rinternals.h>
#include <R_ext/RS.h>

#include <ctype.h>
#include <float.h>
#include <math.h>
#include <float.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "readstat.h"
#include "readstat_iconv.h"
#include "readstat_convert.h"
#include "declared_types.h"
#include "tagged_na.h"
#include "vendor_metadata.h"

#define DTA_113_MAX_INT16 0x7fe4
#define DTA_113_MAX_INT32 0x7fffffe4
#define DTA_113_MISSING_INT8 0x65
#define DTA_OLD_MAX_INT16 0x7ffe
#define DTA_OLD_MAX_INT32 0x7ffffffe

typedef struct {
    char *value;
    readstat_string_ref_t *ref;
} StringRefEntry;

typedef struct {
    int row;
    const char *code;
} MissingEntry;

typedef struct {
    double code;
    char tag;
} ForeignMissingCode;

typedef struct {
    MissingEntry *entries;
    R_xlen_t length;
    R_xlen_t cursor;
} MissingMap;

typedef struct {
    ForeignMissingCode *entries;
    R_xlen_t length;
} ForeignMissingMap;

typedef struct {
    FileExt ext;
    FileVendor vendor;
    int version;
    int strl_threshold;
    SEXP data;
    FILE *out;
    const char *path;
    SEXP dictionary;
    readstat_writer_t *writer;
    StringRefEntry *string_refs;
    size_t string_ref_n;
    size_t string_ref_cap;
    MissingMap *missing_maps;
    R_xlen_t missing_map_n;
    ForeignMissingMap *foreign_maps;
    R_xlen_t foreign_map_n;
    ForeignMissingMap foreign_dictionary;
    int has_foreign_dictionary;
} WriterCtx;

static const char *string_utf8_elt(SEXP x, R_xlen_t i) {
    return Rf_translateCharUTF8(STRING_ELT(x, i));
}

static int string_is_missing(SEXP x, R_xlen_t i) {
    return STRING_ELT(x, i) == NA_STRING;
}

static int string_len_missing(SEXP x, R_xlen_t i) {
    if (string_is_missing(x, i)) {
        return 0;
    }
    return (int) strlen(string_utf8_elt(x, i));
}

static const char *string_elt_or_null(SEXP x, R_xlen_t i) {
    if (TYPEOF(x) != STRSXP || i < 0 || i >= XLENGTH(x) || STRING_ELT(x, i) == NA_STRING) {
        return NULL;
    }
    return CHAR(STRING_ELT(x, i));
}

static int missing_entry_cmp(const void *a, const void *b) {
    const MissingEntry *ea = (const MissingEntry *) a;
    const MissingEntry *eb = (const MissingEntry *) b;

    if (ea->row < eb->row) {
        return -1;
    }
    if (ea->row > eb->row) {
        return 1;
    }
    return 0;
}

static void init_missing_map(MissingMap *map, SEXP x) {
    SEXP na_index = Rf_getAttrib(x, Rf_install("na_index"));
    SEXP names;
    R_xlen_t i;
    R_xlen_t valid = 0;

    memset(map, 0, sizeof(*map));

    if (TYPEOF(na_index) != INTSXP || Rf_xlength(na_index) == 0) {
        return;
    }

    names = Rf_getAttrib(na_index, R_NamesSymbol);
    if (TYPEOF(names) != STRSXP || Rf_xlength(names) != Rf_xlength(na_index)) {
        return;
    }

    for (i = 0; i < Rf_xlength(na_index); ++i) {
        if (INTEGER(na_index)[i] > 0 && STRING_ELT(names, i) != NA_STRING) {
            valid++;
        }
    }

    if (valid == 0) {
        return;
    }

    map->entries = (MissingEntry *) R_Calloc((size_t) valid, MissingEntry);
    map->length = valid;

    valid = 0;
    for (i = 0; i < Rf_xlength(na_index); ++i) {
        if (INTEGER(na_index)[i] > 0 && STRING_ELT(names, i) != NA_STRING) {
            map->entries[valid].row = INTEGER(na_index)[i];
            map->entries[valid].code = CHAR(STRING_ELT(names, i));
            valid++;
        }
    }

    qsort(map->entries, (size_t) map->length, sizeof(MissingEntry), missing_entry_cmp);
}

static void init_missing_maps(WriterCtx *ctx) {
    R_xlen_t j;

    ctx->missing_map_n = (R_xlen_t) Rf_length(ctx->data);
    if (ctx->missing_map_n == 0) {
        return;
    }

    ctx->missing_maps = (MissingMap *) R_Calloc((size_t) ctx->missing_map_n, MissingMap);
    for (j = 0; j < ctx->missing_map_n; ++j) {
        init_missing_map(&ctx->missing_maps[j], VECTOR_ELT(ctx->data, j));
    }
}

static const char *na_index_code(WriterCtx *ctx, int column, R_xlen_t row) {
    MissingMap *map;
    int target_row = (int) (row + 1);

    if (ctx->missing_maps == NULL || column < 0 || column >= ctx->missing_map_n) {
        return NULL;
    }

    map = &ctx->missing_maps[column];
    while (map->cursor < map->length && map->entries[map->cursor].row < target_row) {
        map->cursor++;
    }

    if (map->cursor < map->length && map->entries[map->cursor].row == target_row) {
        return map->entries[map->cursor].code;
    }

    return NULL;
}

static int parse_code_int(const char *code, int *value) {
    char *endptr = NULL;
    long parsed;

    if (code == NULL || code[0] == '\0') {
        return 0;
    }

    parsed = strtol(code, &endptr, 10);
    if (endptr == code || *endptr != '\0') {
        return 0;
    }

    *value = (int) parsed;
    return 1;
}

static int parse_code_double(const char *code, double *value) {
    char *endptr = NULL;
    double parsed;

    if (code == NULL || code[0] == '\0') {
        return 0;
    }

    parsed = strtod(code, &endptr);
    if (endptr == code || *endptr != '\0') {
        return 0;
    }

    *value = parsed;
    return 1;
}

static int foreign_map_entry_cmp_desc(const void *a, const void *b) {
    const double da = *(const double *) a;
    const double db = *(const double *) b;
    if (da < db) {
        return 1;
    }
    if (da > db) {
        return -1;
    }
    return 0;
}

static int foreign_missing_code_cmp(const void *a, const void *b) {
    const ForeignMissingCode *ea = (const ForeignMissingCode *) a;
    const ForeignMissingCode *eb = (const ForeignMissingCode *) b;

    if (ea->code < eb->code) {
        return -1;
    }
    if (ea->code > eb->code) {
        return 1;
    }
    return 0;
}

static int vendor_uses_tagged_missing(FileVendor vendor) {
    return vendor == DECLARED_STATA || vendor == DECLARED_SAS;
}

static char vendor_missing_tag(FileVendor vendor, char tag) {
    if (vendor == DECLARED_SAS) {
        return (char) toupper((unsigned char) tag);
    }
    return (char) tolower((unsigned char) tag);
}

static int foreign_tag_from_code(WriterCtx *ctx, int column, const char *code, char *tag_out, int strict) {
    ForeignMissingMap *map;
    double code_value;
    R_xlen_t i;

    if (!vendor_uses_tagged_missing(ctx->vendor) ||
        ctx->foreign_maps == NULL ||
        column < 0 || column >= ctx->foreign_map_n ||
        code == NULL || code[0] == '\0') {
        return 0;
    }

    if (strlen(code) == 1 &&
        (isalpha((unsigned char) code[0]) || (ctx->vendor == DECLARED_SAS && code[0] == '_'))) {
        *tag_out = vendor_missing_tag(ctx->vendor, code[0]);
        return 1;
    }

    if (!parse_code_double(code, &code_value)) {
        return 0;
    }

    if (ctx->has_foreign_dictionary && ctx->foreign_dictionary.length > 0) {
        map = &ctx->foreign_dictionary;
        for (i = 0; i < map->length; ++i) {
            if (map->entries[i].code == code_value) {
                *tag_out = vendor_missing_tag(ctx->vendor, map->entries[i].tag);
                return 1;
            }
        }
        if (strict) {
            Rf_error("Missing value code '%s' is not present in the export dictionary.", code);
        }
        return 0;
    }

    map = &ctx->foreign_maps[column];
    for (i = 0; i < map->length; ++i) {
        if (map->entries[i].code == code_value) {
            *tag_out = vendor_missing_tag(ctx->vendor, map->entries[i].tag);
            return 1;
        }
    }

    return 0;
}

static void foreign_map_add_code(double *codes, R_xlen_t *n_codes, R_xlen_t cap, double value) {
    R_xlen_t i;
    for (i = 0; i < *n_codes; ++i) {
        if (codes[i] == value) {
            return;
        }
    }
    if (*n_codes < cap) {
        codes[*n_codes] = value;
        (*n_codes)++;
    }
}

static void init_foreign_missing_map(ForeignMissingMap *map, SEXP x, int preserve_sas_tags) {
    SEXP na_index = Rf_getAttrib(x, Rf_install("na_index"));
    SEXP na_values = Rf_getAttrib(x, Rf_install("na_values"));
    SEXP names;
    R_xlen_t cap;
    R_xlen_t n_codes = 0;
    double *codes;
    R_xlen_t i;

    memset(map, 0, sizeof(*map));

    cap = 0;
    if (TYPEOF(na_index) == INTSXP && XLENGTH(na_index) > 0) {
        cap += XLENGTH(na_index);
    }
    if ((TYPEOF(na_values) == REALSXP || TYPEOF(na_values) == INTSXP || TYPEOF(na_values) == STRSXP) &&
        XLENGTH(na_values) > 0) {
        cap += XLENGTH(na_values);
    }
    if (cap == 0) {
        return;
    }

    codes = (double *) R_alloc((size_t) cap, sizeof(double));

    if (TYPEOF(na_index) == INTSXP && XLENGTH(na_index) > 0) {
        names = Rf_getAttrib(na_index, R_NamesSymbol);
        if (TYPEOF(names) == STRSXP && XLENGTH(names) == XLENGTH(na_index)) {
            for (i = 0; i < XLENGTH(names); ++i) {
                double value;
                const char *code = string_elt_or_null(names, i);
                if (parse_code_double(code, &value)) {
                    foreign_map_add_code(codes, &n_codes, cap, value);
                }
            }
        }
    }

    if (TYPEOF(na_values) == REALSXP) {
        for (i = 0; i < XLENGTH(na_values); ++i) {
            if (!ISNA(REAL(na_values)[i]) && !ISNAN(REAL(na_values)[i])) {
                foreign_map_add_code(codes, &n_codes, cap, REAL(na_values)[i]);
            }
        }
    } else if (TYPEOF(na_values) == INTSXP) {
        for (i = 0; i < XLENGTH(na_values); ++i) {
            if (INTEGER(na_values)[i] != NA_INTEGER) {
                foreign_map_add_code(codes, &n_codes, cap, INTEGER(na_values)[i]);
            }
        }
    } else if (TYPEOF(na_values) == STRSXP) {
        for (i = 0; i < XLENGTH(na_values); ++i) {
            double value;
            const char *code = string_elt_or_null(na_values, i);
            if (parse_code_double(code, &value)) {
                foreign_map_add_code(codes, &n_codes, cap, value);
            }
        }
    }

    if (n_codes == 0) {
        return;
    }

    if (n_codes > 26) {
        Rf_error("Cannot export more than 26 distinct missing value codes in one variable to Stata or SAS.");
    }

    qsort(codes, (size_t) n_codes, sizeof(double), foreign_map_entry_cmp_desc);

    map->entries = (ForeignMissingCode *) R_Calloc((size_t) n_codes, ForeignMissingCode);
    map->length = n_codes;
    int used[26] = { 0 };
    for (i = 0; i < n_codes; ++i) {
        map->entries[i].code = codes[i];
        if (preserve_sas_tags && codes[i] >= -116 && codes[i] <= -91 && codes[i] == trunc(codes[i])) {
            int tag = (int)(-91 - codes[i]);
            map->entries[i].tag = 'a' + tag;
            used[tag] = 1;
        }
    }
    int next_tag = 0;
    for (i = 0; i < n_codes; ++i) {
        if (map->entries[i].tag)
            continue;
        while (used[next_tag])
            next_tag++;
        map->entries[i].tag = 'a' + next_tag;
        used[next_tag] = 1;
    }
}

static void init_foreign_missing_maps(WriterCtx *ctx) {
    R_xlen_t j;

    if (!vendor_uses_tagged_missing(ctx->vendor)) {
        return;
    }

    ctx->foreign_map_n = (R_xlen_t) Rf_length(ctx->data);
    if (ctx->foreign_map_n == 0) {
        return;
    }

    ctx->foreign_maps = (ForeignMissingMap *) R_Calloc((size_t) ctx->foreign_map_n, ForeignMissingMap);
    for (j = 0; j < ctx->foreign_map_n; ++j) {
        init_foreign_missing_map(&ctx->foreign_maps[j], VECTOR_ELT(ctx->data, j),
            ctx->vendor == DECLARED_SAS);
    }
}

static SEXP data_frame_column(SEXP df, const char *name) {
    SEXP names = Rf_getAttrib(df, R_NamesSymbol);
    R_xlen_t i;

    if (TYPEOF(df) != VECSXP || TYPEOF(names) != STRSXP) {
        return R_NilValue;
    }

    for (i = 0; i < XLENGTH(df); ++i) {
        if (STRING_ELT(names, i) != NA_STRING && strcmp(CHAR(STRING_ELT(names, i)), name) == 0) {
            return VECTOR_ELT(df, i);
        }
    }

    return R_NilValue;
}

static void init_foreign_dictionary(WriterCtx *ctx, SEXP dictionary) {
    SEXP old;
    SEXP new;
    R_xlen_t i;
    R_xlen_t n;

    memset(&ctx->foreign_dictionary, 0, sizeof(ctx->foreign_dictionary));
    ctx->has_foreign_dictionary = 0;

    if (!vendor_uses_tagged_missing(ctx->vendor) || dictionary == R_NilValue || TYPEOF(dictionary) == NILSXP) {
        return;
    }

    old = data_frame_column(dictionary, "old");
    new = data_frame_column(dictionary, "new");

    if (old == R_NilValue || new == R_NilValue) {
        Rf_error("Export dictionary must contain 'old' and 'new' columns.");
    }

    n = XLENGTH(old);
    if (XLENGTH(new) != n) {
        Rf_error("Export dictionary columns 'old' and 'new' must have the same length.");
    }
    if (n == 0) {
        ctx->has_foreign_dictionary = 1;
        return;
    }

    ctx->foreign_dictionary.entries = (ForeignMissingCode *) R_Calloc((size_t) n, ForeignMissingCode);
    ctx->foreign_dictionary.length = n;
    ctx->has_foreign_dictionary = 1;

    for (i = 0; i < n; ++i) {
        double code_value;
        char tag_value;

        if (TYPEOF(old) == REALSXP) {
            code_value = REAL(old)[i];
        } else if (TYPEOF(old) == INTSXP) {
            code_value = INTEGER(old)[i];
        } else if (TYPEOF(old) == STRSXP) {
            const char *code = string_elt_or_null(old, i);
            if (!parse_code_double(code, &code_value)) {
                Rf_error("Export dictionary 'old' values must be numeric codes.");
            }
        } else {
            Rf_error("Export dictionary 'old' column must be numeric or character.");
        }

        if (!R_FINITE(code_value)) {
            Rf_error("Export dictionary 'old' values must be finite.");
        }

        if (TYPEOF(new) == STRSXP) {
            const char *tag = string_elt_or_null(new, i);
            if (tag == NULL || strlen(tag) != 1 || !isalpha((unsigned char) tag[0])) {
                Rf_error("Export dictionary 'new' values must be single letters.");
            }
            tag_value = (char) tolower((unsigned char) tag[0]);
        } else {
            Rf_error("Export dictionary 'new' column must be character.");
        }

        ctx->foreign_dictionary.entries[i].code = code_value;
        ctx->foreign_dictionary.entries[i].tag = tag_value;
    }

    qsort(
        ctx->foreign_dictionary.entries,
        (size_t) ctx->foreign_dictionary.length,
        sizeof(ForeignMissingCode),
        foreign_missing_code_cmp
    );

    for (i = 1; i < ctx->foreign_dictionary.length; ++i) {
        if (ctx->foreign_dictionary.entries[i - 1].code == ctx->foreign_dictionary.entries[i].code &&
            ctx->foreign_dictionary.entries[i - 1].tag != ctx->foreign_dictionary.entries[i].tag) {
            Rf_error("Export dictionary maps the same missing code to multiple tags.");
        }
    }
}

static enum readstat_measure_e measureType(SEXP x) {
    if (Rf_inherits(x, "ordered")) {
        return READSTAT_MEASURE_ORDINAL;
    }
    if (Rf_inherits(x, "factor")) {
        return READSTAT_MEASURE_NOMINAL;
    }
    switch (TYPEOF(x)) {
        case INTSXP:
        case REALSXP:
            return READSTAT_MEASURE_SCALE;
        case LGLSXP:
        case STRSXP:
            return READSTAT_MEASURE_NOMINAL;
        default:
            return READSTAT_MEASURE_UNKNOWN;
    }
}

static int scalar_int_attr(SEXP x, const char *name) {
    SEXP obj = Rf_getAttrib(x, Rf_install(name));
    if (TYPEOF(obj) == INTSXP && Rf_length(obj) > 0) {
        return INTEGER(obj)[0];
    }
    if (TYPEOF(obj) == REALSXP && Rf_length(obj) > 0) {
        return (int) REAL(obj)[0];
    }
    return 0;
}

static void set_vendor_variable_metadata(WriterCtx *ctx, readstat_variable_t *var, SEXP x) {
    SEXP value;
    if (ctx->vendor == DECLARED_SPSS) {
        value = ddiwr_metadata_field(x, ctx->vendor, "measure");
        if (TYPEOF(value) == STRSXP && XLENGTH(value) == 1) {
            const char *measure = string_utf8_elt(value, 0);
            readstat_variable_set_measure(var, !strcmp(measure, "nominal") ? READSTAT_MEASURE_NOMINAL :
                !strcmp(measure, "ordinal") ? READSTAT_MEASURE_ORDINAL : READSTAT_MEASURE_SCALE);
        }
        value = ddiwr_metadata_field(x, ctx->vendor, "alignment");
        if (TYPEOF(value) == STRSXP && XLENGTH(value) == 1) {
            const char *alignment = string_utf8_elt(value, 0);
            readstat_variable_set_alignment(var, !strcmp(alignment, "left") ? READSTAT_ALIGNMENT_LEFT :
                !strcmp(alignment, "center") ? READSTAT_ALIGNMENT_CENTER : READSTAT_ALIGNMENT_RIGHT);
        }
    } else if (ctx->vendor == DECLARED_SAS) {
        value = ddiwr_metadata_field(x, ctx->vendor, "informat");
        if (TYPEOF(value) == STRSXP && XLENGTH(value) == 1) {
            readstat_variable_set_informat(var, string_utf8_elt(value, 0));
        }
    }
}

static const char *var_label(SEXP x) {
    SEXP label = Rf_getAttrib(x, Rf_install("label"));
    if (TYPEOF(label) == STRSXP && Rf_length(label) > 0) {
        return string_utf8_elt(label, 0);
    }
    return NULL;
}

static const char *var_format(SEXP x, FileVendor vendor) {
    SEXP format = Rf_getAttrib(x, Rf_install(formatAttribute(vendor)));
    VarType type;

    if (TYPEOF(format) == STRSXP && Rf_length(format) > 0) {
        return string_utf8_elt(format, 0);
    }

    type = numTypeFromSEXP(x);
    switch (type) {
        case DECLARED_DATETIME:
            switch (vendor) {
                case DECLARED_SAS: return "DATETIME";
                case DECLARED_SPSS: return "DATETIME";
                case DECLARED_STATA: return "%tc";
            }
            break;
        case DECLARED_DATE:
            switch (vendor) {
                case DECLARED_SAS: return "DATE";
                case DECLARED_SPSS: return "DATE";
                case DECLARED_STATA: return "%td";
            }
            break;
        case DECLARED_TIME:
            switch (vendor) {
                case DECLARED_SAS: return "TIME";
                case DECLARED_SPSS: return "TIME";
                case DECLARED_STATA: return NULL;
            }
            break;
        default:
            break;
    }

    return NULL;
}

static void ensure_string_ref_capacity(WriterCtx *ctx) {
    if (ctx->string_ref_n >= ctx->string_ref_cap) {
        ctx->string_ref_cap = ctx->string_ref_cap == 0 ? 16 : ctx->string_ref_cap * 2;
        ctx->string_refs = (StringRefEntry *) R_Realloc(ctx->string_refs, ctx->string_ref_cap, StringRefEntry);
    }
}

static readstat_string_ref_t *get_or_create_string_ref(WriterCtx *ctx, const char *value) {
    size_t i;
    readstat_string_ref_t *reference;

    for (i = 0; i < ctx->string_ref_n; ++i) {
        if (strcmp(ctx->string_refs[i].value, value) == 0) {
            return ctx->string_refs[i].ref;
        }
    }

    reference = readstat_add_string_ref(ctx->writer, value);
    if (reference == NULL) {
        Rf_error("Failed to register a Stata string reference before writing data.");
    }

    ensure_string_ref_capacity(ctx);
    ctx->string_refs[ctx->string_ref_n].value = (char *) R_Calloc(strlen(value) + 1, char);
    strcpy(ctx->string_refs[ctx->string_ref_n].value, value);
    ctx->string_refs[ctx->string_ref_n].ref = reference;
    ctx->string_ref_n++;
    return ctx->string_refs[ctx->string_ref_n - 1].ref;
}

static void writer_ctx_free(WriterCtx *ctx) {
    size_t i;
    R_xlen_t j;
    if (ctx->out != NULL) {
        fclose(ctx->out);
    }
    if (ctx->writer != NULL) {
        readstat_writer_free(ctx->writer);
    }
    for (i = 0; i < ctx->string_ref_n; ++i) {
        R_Free(ctx->string_refs[i].value);
    }
    if (ctx->string_refs != NULL) {
        R_Free(ctx->string_refs);
    }
    if (ctx->missing_maps != NULL) {
        for (j = 0; j < ctx->missing_map_n; ++j) {
            if (ctx->missing_maps[j].entries != NULL) {
                R_Free(ctx->missing_maps[j].entries);
            }
        }
        R_Free(ctx->missing_maps);
    }
    if (ctx->foreign_maps != NULL) {
        for (j = 0; j < ctx->foreign_map_n; ++j) {
            if (ctx->foreign_maps[j].entries != NULL) {
                R_Free(ctx->foreign_maps[j].entries);
            }
        }
        R_Free(ctx->foreign_maps);
    }
    if (ctx->foreign_dictionary.entries != NULL) {
        R_Free(ctx->foreign_dictionary.entries);
    }
}

static void check_status(readstat_error_t err, const char *context) {
    if (err != READSTAT_OK) {
        Rf_error("%s: %s", context, readstat_error_message(err));
    }
}

static ssize_t data_writer(const void *data, size_t len, void *ctx_) {
    WriterCtx *ctx = (WriterCtx *) ctx_;
    return fwrite(data, sizeof(char), len, ctx->out);
}

static void writer_ctx_init(WriterCtx *ctx, FileExt ext, SEXP data, SEXP path) {
    memset(ctx, 0, sizeof(*ctx));
    ctx->ext = ext;
    ctx->vendor = extVendor(ext);
    ctx->version = 0;
    ctx->strl_threshold = 2045;
    ctx->data = data;
    ctx->path = CHAR(STRING_ELT(path, 0));
    ctx->writer = readstat_writer_init();
    check_status(readstat_set_data_writer(ctx->writer, data_writer), "Failed to initialize writer");
}

static void set_compression(WriterCtx *ctx, const char *compress) {
    if (strcmp(compress, "zsav") == 0) {
        readstat_writer_set_compression(ctx->writer, READSTAT_COMPRESS_BINARY);
    } else if (strcmp(compress, "none") == 0) {
        readstat_writer_set_compression(ctx->writer, READSTAT_COMPRESS_NONE);
    } else {
        readstat_writer_set_compression(ctx->writer, READSTAT_COMPRESS_ROWS);
    }
}

static void set_file_label(WriterCtx *ctx, SEXP label) {
    if (TYPEOF(label) == STRSXP && Rf_length(label) > 0 && STRING_ELT(label, 0) != NA_STRING) {
        readstat_writer_set_file_label(ctx->writer, string_utf8_elt(label, 0));
    }
}

static readstat_label_set_t *build_numeric_label_set(WriterCtx *ctx, SEXP x, const char *name, int as_int, int column) {
    SEXP labels = Rf_getAttrib(x, Rf_install("labels"));
    SEXP nms;
    readstat_label_set_t *set;
    R_xlen_t i;
    if (TYPEOF(labels) == NILSXP || Rf_length(labels) == 0) {
        return NULL;
    }
    nms = Rf_getAttrib(labels, R_NamesSymbol);
    set = readstat_add_label_set(ctx->writer, as_int ? READSTAT_TYPE_INT32 : READSTAT_TYPE_DOUBLE, name);
    if (TYPEOF(labels) == INTSXP) {
        for (i = 0; i < Rf_xlength(labels); ++i) {
            char export_tag;
            char code_buf[32];
            snprintf(code_buf, sizeof(code_buf), "%d", INTEGER(labels)[i]);
            if (foreign_tag_from_code(ctx, column, code_buf, &export_tag, 0)) {
                readstat_label_tagged_value(set, export_tag, string_utf8_elt(nms, i));
            } else
            if (as_int) {
                readstat_label_int32_value(set, INTEGER(labels)[i], string_utf8_elt(nms, i));
            } else {
                readstat_label_double_value(set, (double) INTEGER(labels)[i], string_utf8_elt(nms, i));
            }
        }
    } else if (TYPEOF(labels) == REALSXP) {
        for (i = 0; i < Rf_xlength(labels); ++i) {
            char tag = tagged_na_value(REAL(labels)[i]);
            if (!ISNAN(REAL(labels)[i]) || tag == '\0') {
                char export_tag;
                char code_buf[64];
                snprintf(code_buf, sizeof(code_buf), "%.15g", REAL(labels)[i]);
                if (foreign_tag_from_code(ctx, column, code_buf, &export_tag, 0)) {
                    readstat_label_tagged_value(set, export_tag, string_utf8_elt(nms, i));
                } else
                if (as_int) {
                    readstat_label_int32_value(set, (int) REAL(labels)[i], string_utf8_elt(nms, i));
                } else {
                    readstat_label_double_value(set, REAL(labels)[i], string_utf8_elt(nms, i));
                }
            } else {
                if (ctx->ext == DECLARED_XPT || ctx->ext == DECLARED_SAS7BDAT || ctx->ext == DECLARED_SAS7BCAT) {
                    tag = (char) toupper((unsigned char) tag);
                }
                readstat_label_tagged_value(set, tag, string_utf8_elt(nms, i));
            }
        }
    } else if (TYPEOF(labels) == STRSXP) {
        char *endptr = NULL;
        for (i = 0; i < Rf_xlength(labels); ++i) {
            const char *value = string_utf8_elt(labels, i);
            if (strlen(value) == 1 && isalpha((unsigned char) value[0]) && !as_int) {
                char tag = (char) tolower((unsigned char) value[0]);
                if (ctx->ext == DECLARED_XPT || ctx->ext == DECLARED_SAS7BDAT || ctx->ext == DECLARED_SAS7BCAT) {
                    tag = (char) toupper((unsigned char) tag);
                }
                readstat_label_tagged_value(set, tag, string_utf8_elt(nms, i));
            } else if (as_int) {
                long iv = strtol(value, &endptr, 10);
                if (endptr != NULL && *endptr == '\0') {
                    readstat_label_int32_value(set, (int) iv, string_utf8_elt(nms, i));
                }
            } else {
                double dv = strtod(value, &endptr);
                if (endptr != NULL && *endptr == '\0') {
                    readstat_label_double_value(set, dv, string_utf8_elt(nms, i));
                }
            }
        }
    }
    return set;
}

static int double_is_intish(double value) {
    return R_finite(value) && fabs(value - round(value)) < sqrt(DBL_EPSILON);
}

static readstat_type_t dta_numeric_type_for_column(WriterCtx *ctx, SEXP x) {
    R_xlen_t i;
    double min_value = 0.0;
    double max_value = 0.0;
    int seen = 0;
    int all_intish = 1;
    double int8_max = ctx->version >= 113 ? (double) (DTA_113_MISSING_INT8 - 1) : 126.0;
    double int16_max = ctx->version >= 113 ? (double) DTA_113_MAX_INT16 : (double) DTA_OLD_MAX_INT16;
    double int32_max = ctx->version >= 113 ? (double) DTA_113_MAX_INT32 : (double) DTA_OLD_MAX_INT32;

    for (i = 0; i < Rf_xlength(x); ++i) {
        double value;
        if (TYPEOF(x) == INTSXP) {
            if (INTEGER(x)[i] == NA_INTEGER) {
                continue;
            }
            value = INTEGER(x)[i];
        } else {
            value = REAL(x)[i];
        }
        if (!R_finite(value)) {
            continue;
        }
        /* Storage bounds and integer precision apply to the exported units. */
        value = adjustDatetimeFromR(ctx->vendor, x, value);
        if (!double_is_intish(value)) {
            all_intish = 0;
            break;
        }
        if (!seen) {
            min_value = max_value = value;
            seen = 1;
        } else {
            if (value < min_value) {
                min_value = value;
            }
            if (value > max_value) {
                max_value = value;
            }
        }
    }

    if (!all_intish) {
        return READSTAT_TYPE_DOUBLE;
    }
    if (!seen) {
        return READSTAT_TYPE_INT8;
    }
    if (min_value >= -127.0 && max_value <= int8_max) {
        return READSTAT_TYPE_INT8;
    }
    if (min_value >= -32767.0 && max_value <= int16_max) {
        return READSTAT_TYPE_INT16;
    }
    if (min_value >= -2147483647.0 && max_value <= int32_max) {
        return READSTAT_TYPE_INT32;
    }
    return READSTAT_TYPE_DOUBLE;
}

static readstat_type_t requested_dta_type(WriterCtx *ctx, SEXP x, readstat_type_t fallback) {
    SEXP field = ddiwr_metadata_field(x, DECLARED_STATA, "type");
    if (ctx->ext != DECLARED_DTA || TYPEOF(field) != STRSXP || XLENGTH(field) != 1) {
        return fallback;
    }
    const char *type = string_utf8_elt(field, 0);
    readstat_type_t result = READSTAT_TYPE_DOUBLE;
    double minimum = -0x1.fffffffffffffp+1022, maximum = 0x1.fffffffffffffp+1022;
    int integer = 0;
    if (!strcmp(type, "byte")) {
        result = READSTAT_TYPE_INT8; minimum = -127; maximum = 100; integer = 1;
    } else if (!strcmp(type, "int")) {
        result = READSTAT_TYPE_INT16; minimum = -32767; maximum = DTA_113_MAX_INT16; integer = 1;
    } else if (!strcmp(type, "long")) {
        result = READSTAT_TYPE_INT32; minimum = -2147483647; maximum = DTA_113_MAX_INT32; integer = 1;
    } else if (!strcmp(type, "float")) {
        result = READSTAT_TYPE_FLOAT; minimum = -0x1.fffffep+126; maximum = 0x1.fffffep+126;
    }

    for (R_xlen_t i = 0; i < XLENGTH(x); i++) {
        double value = TYPEOF(x) == REALSXP ? REAL(x)[i] :
            TYPEOF(x) == LGLSXP ? LOGICAL(x)[i] : INTEGER(x)[i];
        if ((TYPEOF(x) != REALSXP && value == NA_INTEGER) || !R_finite(value)) {
            continue;
        }
        value = adjustDatetimeFromR(ctx->vendor, x, value);
        if (value < minimum || value > maximum ||
            (integer && value != trunc(value)) ||
            (result == READSTAT_TYPE_FLOAT && (double)(float)value != value)) {
            Rf_error("'stata_metadata$type = %s' cannot represent the exported values without loss. Clear 'type' to select storage automatically.", type);
        }
    }
    return result;
}

static readstat_label_set_t *build_string_label_set(WriterCtx *ctx, SEXP x, const char *name) {
    SEXP labels = Rf_getAttrib(x, Rf_install("labels"));
    SEXP nms;
    readstat_label_set_t *set;
    R_xlen_t i;
    if (TYPEOF(labels) != STRSXP || Rf_length(labels) == 0) {
        return NULL;
    }
    nms = Rf_getAttrib(labels, R_NamesSymbol);
    set = readstat_add_label_set(ctx->writer, READSTAT_TYPE_STRING, name);
    for (i = 0; i < Rf_xlength(labels); ++i) {
        readstat_label_string_value(set, string_utf8_elt(labels, i), string_utf8_elt(nms, i));
    }
    return set;
}

static void add_missing_definition(WriterCtx *ctx, readstat_variable_t *var, SEXP x) {
    SEXP na_range = Rf_getAttrib(x, Rf_install("na_range"));
    SEXP na_values = Rf_getAttrib(x, Rf_install("na_values"));
    R_xlen_t i;

    if (ctx->vendor != DECLARED_SPSS) {
        return;
    }

    if (TYPEOF(na_range) == REALSXP && Rf_length(na_range) == 2) {
        readstat_variable_add_missing_double_range(var, REAL(na_range)[0], REAL(na_range)[1]);
    } else if (TYPEOF(na_range) == INTSXP && Rf_length(na_range) == 2) {
        readstat_variable_add_missing_double_range(var, INTEGER(na_range)[0], INTEGER(na_range)[1]);
    } else if (TYPEOF(na_range) == STRSXP && Rf_length(na_range) == 2) {
        readstat_variable_add_missing_string_range(var, CHAR(STRING_ELT(na_range, 0)), CHAR(STRING_ELT(na_range, 1)));
    }

    if (TYPEOF(na_values) == REALSXP) {
        for (i = 0; i < Rf_xlength(na_values); ++i) {
            readstat_variable_add_missing_double_value(var, REAL(na_values)[i]);
        }
    } else if (TYPEOF(na_values) == INTSXP) {
        for (i = 0; i < Rf_xlength(na_values); ++i) {
            readstat_variable_add_missing_double_value(var, INTEGER(na_values)[i]);
        }
    } else if (TYPEOF(na_values) == STRSXP) {
        for (i = 0; i < Rf_xlength(na_values); ++i) {
            readstat_variable_add_missing_string_value(var, CHAR(STRING_ELT(na_values, i)));
        }
    }
}

static readstat_error_t define_variable_int(WriterCtx *ctx, SEXP x, const char *name, int column) {
    readstat_label_set_t *label_set = NULL;
    readstat_variable_t *var;
    const char *format = var_format(x, ctx->vendor);
    readstat_type_t variable_type = READSTAT_TYPE_INT32;

    if (numTypeFromSEXP(x) != DECLARED_DEFAULT) {
        if (ctx->ext == DECLARED_DTA) {
            variable_type = dta_numeric_type_for_column(ctx, x);
        } else {
            /* SPSS seconds and SAS epoch offsets can exceed int32. */
            variable_type = READSTAT_TYPE_DOUBLE;
        }
    }

    variable_type = requested_dta_type(ctx, x, variable_type);

    if (Rf_inherits(x, "factor")) {
        SEXP levels = Rf_getAttrib(x, Rf_install("levels"));
        R_xlen_t i;
        label_set = readstat_add_label_set(ctx->writer, READSTAT_TYPE_INT32, name);
        for (i = 0; i < Rf_xlength(levels); ++i) {
            readstat_label_int32_value(label_set, (int) i + 1, string_utf8_elt(levels, i));
        }
    } else {
        label_set = build_numeric_label_set(ctx, x, name,
            ctx->ext == DECLARED_DTA && variable_type != READSTAT_TYPE_DOUBLE &&
                variable_type != READSTAT_TYPE_FLOAT, column);
    }

    var = readstat_add_variable(ctx->writer, name, variable_type, scalar_int_attr(x, "width"));
    readstat_variable_set_format(var, format);
    readstat_variable_set_label(var, var_label(x));
    readstat_variable_set_label_set(var, label_set);
    readstat_variable_set_measure(var, measureType(x));
    readstat_variable_set_display_width(var, scalar_int_attr(x, "display_width"));
    set_vendor_variable_metadata(ctx, var, x);
    add_missing_definition(ctx, var, x);
    return readstat_validate_variable(ctx->writer, var);
}

static readstat_error_t define_variable_double(WriterCtx *ctx, SEXP x, const char *name, int column) {
    readstat_type_t variable_type = READSTAT_TYPE_DOUBLE;
    int as_int = 0;
    readstat_label_set_t *label_set;
    readstat_variable_t *var;

    if (ctx->ext == DECLARED_DTA) {
        variable_type = requested_dta_type(ctx, x, dta_numeric_type_for_column(ctx, x));
        as_int = variable_type == READSTAT_TYPE_INT8 ||
            variable_type == READSTAT_TYPE_INT16 ||
            variable_type == READSTAT_TYPE_INT32;
    }

    label_set = build_numeric_label_set(ctx, x, name, as_int, column);
    var = readstat_add_variable(ctx->writer, name, variable_type, scalar_int_attr(x, "width"));
    readstat_variable_set_format(var, var_format(x, ctx->vendor));
    readstat_variable_set_label(var, var_label(x));
    readstat_variable_set_label_set(var, label_set);
    readstat_variable_set_measure(var, measureType(x));
    readstat_variable_set_display_width(var, scalar_int_attr(x, "display_width"));
    set_vendor_variable_metadata(ctx, var, x);
    add_missing_definition(ctx, var, x);
    return readstat_validate_variable(ctx->writer, var);
}

static readstat_error_t define_variable_string(WriterCtx *ctx, SEXP x, const char *name) {
    readstat_label_set_t *label_set = build_string_label_set(ctx, x, name);
    SEXP missing_codes = Rf_getAttrib(Rf_getAttrib(x, Rf_install("na_index")), R_NamesSymbol);
    int user_width = scalar_int_attr(x, "width");
    int max_length = 1;
    readstat_variable_t *var;
    R_xlen_t i;

    for (i = 0; i < Rf_xlength(x); ++i) {
        int length = string_len_missing(x, i);
        if (length > max_length) {
            max_length = length;
        }
    }

    /* Missing codes are restored during export and must fit the declared width. */
    for (i = 0; i < Rf_xlength(missing_codes); ++i) {
        int length = string_len_missing(missing_codes, i);
        if (length > max_length) {
            max_length = length;
        }
    }

    if (user_width < max_length) {
        user_width = max_length;
    }

    SEXP storage_type = ddiwr_metadata_field(x, ctx->vendor, "type");
    int force_strl = TYPEOF(storage_type) == STRSXP && XLENGTH(storage_type) == 1 &&
        !strcmp(string_utf8_elt(storage_type, 0), "strL");
    if (ctx->ext == DECLARED_DTA && ctx->version >= 117 && (force_strl || user_width > ctx->strl_threshold)) {
        var = readstat_add_variable(ctx->writer, name, READSTAT_TYPE_STRING_REF, user_width);

        /* Upstream measures strL records before writing the first data row. */
        for (i = 0; i < Rf_xlength(x); ++i) {
            if (!string_is_missing(x, i)) {
                get_or_create_string_ref(ctx, string_utf8_elt(x, i));
            }
        }
        for (i = 0; i < Rf_xlength(missing_codes); ++i) {
            if (!string_is_missing(missing_codes, i)) {
                get_or_create_string_ref(ctx, string_utf8_elt(missing_codes, i));
            }
        }
    } else {
        var = readstat_add_variable(ctx->writer, name, READSTAT_TYPE_STRING, user_width);
    }

    readstat_variable_set_format(var, var_format(x, ctx->vendor));
    readstat_variable_set_label(var, var_label(x));
    readstat_variable_set_label_set(var, label_set);
    readstat_variable_set_measure(var, measureType(x));
    readstat_variable_set_display_width(var, scalar_int_attr(x, "display_width"));
    set_vendor_variable_metadata(ctx, var, x);
    add_missing_definition(ctx, var, x);
    return readstat_validate_variable(ctx->writer, var);
}

static readstat_error_t insert_int_value(WriterCtx *ctx, readstat_variable_t *var, int value, int missing) {
    if (missing) {
        return readstat_insert_missing_value(ctx->writer, var);
    }
    if (var->type == READSTAT_TYPE_FLOAT) {
        return readstat_insert_float_value(ctx->writer, var, (float)value);
    }
    if (var->type == READSTAT_TYPE_INT8) {
        return readstat_insert_int8_value(ctx->writer, var, (int8_t) value);
    }
    if (var->type == READSTAT_TYPE_INT16) {
        return readstat_insert_int16_value(ctx->writer, var, (int16_t) value);
    }
    return readstat_insert_int32_value(ctx->writer, var, (int32_t) value);
}

static readstat_error_t insert_double_value(WriterCtx *ctx, readstat_variable_t *var, double value, int missing) {
    if (missing) {
        char tag = tagged_na_value(value);
        if (tag != '\0') {
            if (ctx->ext == DECLARED_XPT || ctx->ext == DECLARED_SAS7BDAT || ctx->ext == DECLARED_SAS7BCAT) {
                tag = (char) toupper((unsigned char) tag);
            }
            return readstat_insert_tagged_missing_value(ctx->writer, var, tag);
        }
        return readstat_insert_missing_value(ctx->writer, var);
    }
    if (var->type == READSTAT_TYPE_FLOAT) {
        return readstat_insert_float_value(ctx->writer, var, (float)value);
    }
    return readstat_insert_double_value(ctx->writer, var, value);
}

static readstat_error_t writer_insert_string_value(WriterCtx *ctx, readstat_variable_t *var, const char *value, int missing) {
    if (missing) {
        return readstat_insert_missing_value(ctx->writer, var);
    }
    if (var->type == READSTAT_TYPE_STRING_REF) {
        return readstat_insert_string_ref(ctx->writer, var, get_or_create_string_ref(ctx, value));
    }
    return readstat_insert_string_value(ctx->writer, var, value);
}

static void register_spss_attributes(WriterCtx *ctx, SEXP object, readstat_variable_t *variable) {
    SEXP attributes = Rf_getAttrib(object, Rf_install("spss.attributes"));
    SEXP names = Rf_getAttrib(attributes, R_NamesSymbol);
    R_xlen_t i, j;
    if (attributes == R_NilValue) {
        return;
    }
    for (i = 0; i < XLENGTH(attributes); i++) {
        SEXP values = VECTOR_ELT(attributes, i);
        const char **strings = (const char **)R_alloc(XLENGTH(values), sizeof(char *));
        for (j = 0; j < XLENGTH(values); j++) {
            strings[j] = string_utf8_elt(values, j);
        }
        check_status(readstat_writer_add_spss_attribute(ctx->writer, variable,
                string_utf8_elt(names, i), strings, XLENGTH(values)),
                "Failed to register SPSS custom attributes");
    }
}

static void register_spss_mr_sets(WriterCtx *ctx) {
    SEXP sets = ddiwr_metadata_field(ctx->data, DECLARED_SPSS, "mr_sets");
    if (sets == R_NilValue) {
        return;
    }
    SEXP names = Rf_getAttrib(sets, R_NamesSymbol);
    for (R_xlen_t i = 0; i < XLENGTH(sets); i++) {
        /* prepare_spss_mr_sets supplies these seven fields in canonical order. */
        SEXP entry = VECTOR_ELT(sets, i);
        SEXP counted = VECTOR_ELT(entry, 4);
        SEXP variables = VECTOR_ELT(entry, 5);
        mr_set_t set = { .name = (char *)string_utf8_elt(names, i),
            .type = string_utf8_elt(VECTOR_ELT(entry, 0), 0)[0],
            .label = (char *)string_utf8_elt(VECTOR_ELT(entry, 1), 0),
            .is_dichotomy = Rf_asLogical(VECTOR_ELT(entry, 2)),
            .counted_string = counted == R_NilValue ? NULL : (char *)string_utf8_elt(counted, 0),
            .num_subvars = XLENGTH(variables),
            .label_from_var_label = Rf_asLogical(VECTOR_ELT(entry, 6)) };
        set.subvariables = (char **)R_alloc(set.num_subvars, sizeof(char *));
        for (int j = 0; j < set.num_subvars; j++) {
            set.subvariables[j] = (char *)string_utf8_elt(variables, j);
        }
        check_status(readstat_writer_add_mr_set(ctx->writer, &set),
            "Failed to register spss_metadata$mr_sets");
    }
}

static const char *stata_metadata_string(WriterCtx *ctx, const char *value) {
    iconv_t converter;
    readstat_error_t status;
    char *encoded;
    size_t length;
    if (ctx->version >= 118) {
        return value;
    }
    length = strlen(value);
    encoded = (char *)R_alloc(length + 1, sizeof(char));
    converter = iconv_open("WINDOWS-1252", "UTF-8");
    if (converter == (iconv_t)-1) {
        Rf_error("Failed to initialize Stata characteristic encoding.");
    }
    status = readstat_convert_notrim(encoded, length + 1, value, length, converter);
    iconv_close(converter);
    check_status(status, "Failed to encode Stata characteristics");
    return encoded;
}

static int register_stata_characteristics(WriterCtx *ctx, SEXP object, readstat_variable_t *variable) {
    SEXP characteristics = Rf_getAttrib(object, Rf_install("stata.characteristics"));
    SEXP names = Rf_getAttrib(characteristics, R_NamesSymbol);
    R_xlen_t i;
    int has_notes = 0;
    if (characteristics == R_NilValue) {
        return 0;
    }
    for (i = 0; i < XLENGTH(characteristics); i++) {
        const char *name = string_utf8_elt(names, i);
        const char *value = string_utf8_elt(VECTOR_ELT(characteristics, i), 0);
        if (!variable && !strncmp(name, "note", 4) && name[4] &&
                strspn(name + 4, "0123456789") == strlen(name + 4)) {
            has_notes = 1;
        }
        check_status(readstat_writer_add_characteristic(ctx->writer, variable,
                stata_metadata_string(ctx, name), stata_metadata_string(ctx, value)),
                "Failed to register Stata characteristics");
    }
    return has_notes;
}

static void writer_write(WriterCtx *ctx, SEXP dictionary) {
    readstat_error_t status = READSTAT_OK;
    int p = Rf_length(ctx->data);
    int n = p > 0 ? Rf_length(VECTOR_ELT(ctx->data, 0)) : 0;
    SEXP names = Rf_getAttrib(ctx->data, R_NamesSymbol);
    int j;
    int i;

    SEXP created = ddiwr_metadata_field(ctx->data, ctx->vendor, "created");
    if (created != R_NilValue) {
        check_status(readstat_writer_set_file_timestamp(ctx->writer, (time_t)Rf_asReal(created)),
            "Failed to set creation timestamp");
    }
    if (ctx->vendor == DECLARED_SAS) {
        SEXP modified = ddiwr_metadata_field(ctx->data, ctx->vendor, "modified");
        if (modified != R_NilValue) {
            check_status(readstat_writer_set_file_modified_timestamp(ctx->writer, (time_t)Rf_asReal(modified)),
                "Failed to set modification timestamp");
        }
    }
    if (ctx->ext == DECLARED_SAS7BDAT) {
        SEXP table_name = ddiwr_metadata_field(ctx->data, ctx->vendor, "table_name");
        if (TYPEOF(table_name) == STRSXP && XLENGTH(table_name) == 1) {
            check_status(readstat_writer_set_table_name(ctx->writer, string_utf8_elt(table_name, 0)),
                "Failed to set SAS table name");
        }
    }

    switch (ctx->ext) {
        case DECLARED_SAV:
            status = readstat_begin_writing_sav(ctx->writer, ctx, n);
            break;
        case DECLARED_DTA:
            status = readstat_begin_writing_dta(ctx->writer, ctx, n);
            break;
        case DECLARED_SAS7BDAT:
            status = readstat_begin_writing_sas7bdat(ctx->writer, ctx, n);
            break;
        case DECLARED_XPT:
            status = readstat_begin_writing_xport(ctx->writer, ctx, n);
            break;
        default:
            break;
    }
    check_status(status, "Failed to create file");

    init_missing_maps(ctx);
    init_foreign_dictionary(ctx, dictionary);
    init_foreign_missing_maps(ctx);

    for (j = 0; j < p; ++j) {
        SEXP col = VECTOR_ELT(ctx->data, j);
        const char *name = string_utf8_elt(names, j);
        switch (TYPEOF(col)) {
            case LGLSXP:
            case INTSXP:
                check_status(define_variable_int(ctx, col, name, j), "Failed to create integer column");
                break;
            case REALSXP:
                check_status(define_variable_double(ctx, col, name, j), "Failed to create numeric column");
                break;
            case STRSXP:
                check_status(define_variable_string(ctx, col, name), "Failed to create string column");
                break;
            default:
                Rf_error("Unsupported column type: %s", Rf_type2char(TYPEOF(col)));
        }
    }

    if (ctx->ext == DECLARED_SAV) {
        SEXP notes = Rf_getAttrib(ctx->data, Rf_install("notes"));
        if (notes != R_NilValue) {
            for (R_xlen_t k = 0; k < XLENGTH(notes); k++) {
                readstat_add_note(ctx->writer, string_utf8_elt(notes, k));
            }
        }
        register_spss_attributes(ctx, ctx->data, NULL);
        register_spss_mr_sets(ctx);
        for (j = 0; j < p; j++) {
            register_spss_attributes(ctx, VECTOR_ELT(ctx->data, j),
                    readstat_get_variable(ctx->writer, j));
        }
    }
    if (ctx->ext == DECLARED_DTA) {
        int has_notes = register_stata_characteristics(ctx, ctx->data, NULL);
        for (j = 0; j < p; j++) {
            register_stata_characteristics(ctx, VECTOR_ELT(ctx->data, j),
                    readstat_get_variable(ctx->writer, j));
        }
        if (!has_notes) {
            SEXP notes = Rf_getAttrib(ctx->data, Rf_install("notes"));
            if (notes != R_NilValue) {
                for (j = 0; j < XLENGTH(notes); j++) {
                    readstat_add_note(ctx->writer, stata_metadata_string(ctx, string_utf8_elt(notes, j)));
                }
            }
        }
    }

    check_status(readstat_validate_metadata(ctx->writer), "Failed to validate metadata");

    /* Define and validate columns before touching the destination. */
    ctx->out = fopen(ctx->path, "wb");
    if (ctx->out == NULL) {
        Rf_error("Failed to open '%s' for writing.", ctx->path);
    }

    for (i = 0; i < n; ++i) {
        check_status(readstat_begin_row(ctx->writer), "Failed to begin row");
        for (j = 0; j < p; ++j) {
            SEXP col = VECTOR_ELT(ctx->data, j);
            readstat_variable_t *var = readstat_get_variable(ctx->writer, j);
            const char *code = na_index_code(ctx, j, i);
            switch (TYPEOF(col)) {
                case LGLSXP: {
                    int value = LOGICAL(col)[i];
                    int missing = value == NA_LOGICAL;
                    int code_int;
                    if (missing && parse_code_int(code, &code_int)) {
                        check_status(insert_int_value(ctx, var, code_int, 0), "Failed to insert logical value");
                    } else {
                        check_status(insert_int_value(ctx, var, value, missing), "Failed to insert logical value");
                    }
                    break;
                }
                case INTSXP: {
                    int value = INTEGER(col)[i];
                    int missing = value == NA_INTEGER;
                    int code_int;
                    char export_tag;
                    if (missing && foreign_tag_from_code(ctx, j, code, &export_tag, 1)) {
                        check_status(readstat_insert_tagged_missing_value(ctx->writer, var, export_tag), "Failed to insert integer value");
                    } else if (missing && parse_code_int(code, &code_int)) {
                        if (var->type == READSTAT_TYPE_DOUBLE || var->type == READSTAT_TYPE_FLOAT) {
                            check_status(insert_double_value(ctx, var, code_int, 0), "Failed to insert integer value");
                        } else {
                            check_status(insert_int_value(ctx, var, code_int, 0), "Failed to insert integer value");
                        }
                    } else if (var->type == READSTAT_TYPE_DOUBLE || var->type == READSTAT_TYPE_FLOAT) {
                        check_status(insert_double_value(ctx, var, missing ? NA_REAL : adjustDatetimeFromR(ctx->vendor, col, value), missing), "Failed to insert integer value");
                    } else {
                        check_status(insert_int_value(ctx, var, (int) adjustDatetimeFromR(ctx->vendor, col, value), missing), "Failed to insert integer value");
                    }
                    break;
                }
                case REALSXP: {
                    double value = REAL(col)[i];
                    double code_double;
                    int code_int;
                    char export_tag;
                    if (!R_finite(value) && foreign_tag_from_code(ctx, j, code, &export_tag, 1)) {
                        check_status(insert_double_value(ctx, var, make_tagged_na((char) tolower((unsigned char) export_tag)), 1), "Failed to insert numeric value");
                    } else if ((var->type == READSTAT_TYPE_INT8 || var->type == READSTAT_TYPE_INT16 || var->type == READSTAT_TYPE_INT32) &&
                               !R_finite(value) && parse_code_int(code, &code_int)) {
                        check_status(insert_int_value(ctx, var, code_int, 0), "Failed to insert numeric value");
                    } else if ((var->type == READSTAT_TYPE_INT8 || var->type == READSTAT_TYPE_INT16 || var->type == READSTAT_TYPE_INT32) &&
                               R_finite(value)) {
                        check_status(insert_int_value(ctx, var, (int) llround(adjustDatetimeFromR(ctx->vendor, col, value)), 0), "Failed to insert numeric value");
                    } else if (!R_finite(value) && parse_code_double(code, &code_double)) {
                        check_status(insert_double_value(ctx, var, code_double, 0), "Failed to insert numeric value");
                    } else if (!R_finite(value) && code != NULL && strlen(code) == 1 && isalpha((unsigned char) code[0])) {
                        check_status(insert_double_value(ctx, var, make_tagged_na((char) tolower((unsigned char) code[0])), 1), "Failed to insert numeric value");
                    } else {
                        check_status(insert_double_value(ctx, var, adjustDatetimeFromR(ctx->vendor, col, value), !R_finite(value)), "Failed to insert numeric value");
                    }
                    break;
                }
                case STRSXP:
                    if (string_is_missing(col, i) && code != NULL) {
                        check_status(writer_insert_string_value(ctx, var, code, 0), "Failed to insert string value");
                    } else {
                        check_status(writer_insert_string_value(ctx, var, string_utf8_elt(col, i), string_is_missing(col, i)), "Failed to insert string value");
                    }
                    break;
                default:
                    break;
            }
        }
        check_status(readstat_end_row(ctx->writer), "Failed to end row");
    }

    check_status(readstat_end_writing(ctx->writer), "Failed to finalize writing");
    if (ctx->ext == DECLARED_SAS7BDAT) {
        FILE *out = ctx->out;
        ctx->out = NULL;
        if (fclose(out) != 0)
            Rf_error("Failed to close SAS data output '%s'.", ctx->path);
    }
}

static SEXP writer_write_call(void *data) {
    WriterCtx *ctx = (WriterCtx *)data;
    writer_write(ctx, ctx->dictionary);
    return R_NilValue;
}

static void writer_cleanup(void *data) {
    writer_ctx_free((WriterCtx *)data);
}

static uint64_t sas_catalog_key(double value, char tag) {
    uint64_t key;
    if (tag)
        return UINT64_C(0x7ff8000000000000) | (unsigned char)tag;
    if (value == 0)
        value = 0; /* Positive and negative zero select the same SAS value. */
    memcpy(&key, &value, sizeof(key));
    return key;
}

static int sas_catalog_key_compare(const void *left, const void *right) {
    uint64_t a = *(const uint64_t *)left, b = *(const uint64_t *)right;
    return (a > b) - (a < b);
}

static int sas_catalog_is_missing_code(WriterCtx *ctx, int column, double value) {
    ForeignMissingMap *map = &ctx->foreign_maps[column];
    for (R_xlen_t i=0; i<map->length; i++) {
        if (map->entries[i].code == value)
            return 1;
    }
    return 0;
}

static void sas_catalog_labels(WriterCtx *ctx, SEXP x, const char *name, int column) {
    if (numTypeFromFormat(DECLARED_SAS, name) != DECLARED_DEFAULT)
        Rf_error("Temporal format names are not supported for numeric SAS catalog labels; use clearFormats().");
    VarType temporal = numTypeFromSEXP(x);
    if (temporal != DECLARED_DEFAULT) {
        SEXP informat = ddiwr_metadata_field(x, DECLARED_SAS, "informat");
        if (TYPEOF(informat) != STRSXP || XLENGTH(informat) != 1 ||
                numTypeFromMetadata(DECLARED_SAS, NULL, string_utf8_elt(informat, 0)) != temporal)
            Rf_error("Labelled temporal SAS columns require an informat matching their date/time class.");
    }
    SEXP labels = Rf_getAttrib(x, Rf_install("labels"));
    SEXP texts = Rf_getAttrib(labels, R_NamesSymbol);
    R_xlen_t count = XLENGTH(labels);
    if ((TYPEOF(labels) != INTSXP && TYPEOF(labels) != REALSXP) ||
            TYPEOF(texts) != STRSXP || XLENGTH(texts) != count)
        Rf_error("SAS catalog labels must be named numeric vectors.");

    SEXP tags = ddiwr_metadata_field(x, DECLARED_SAS, "label_tags");
    int has_tags = tags != R_NilValue;
    if (has_tags && (TYPEOF(tags) != STRSXP || XLENGTH(tags) != count))
        Rf_error("'sas_metadata$label_tags' must match the value-label vector.");

    readstat_label_set_t *set = NULL;
    for (int i=0; i<ctx->writer->label_sets_count; i++) {
        if (!strcmp(ctx->writer->label_sets[i]->name, name)) {
            set = ctx->writer->label_sets[i];
            break;
        }
    }
    int shared = set != NULL;
    if (shared && set->value_labels_count != count)
        Rf_error("Conflicting value-label definitions for SAS format '%s'.", name);
    if (!shared)
        set = readstat_add_label_set(ctx->writer, READSTAT_TYPE_DOUBLE, name);

    uint64_t *keys = (uint64_t *)R_alloc(count, sizeof(uint64_t));
    int width = scalar_int_attr(x, "width");
    if (width != 0 && (width < 3 || width > 8))
        Rf_error("Numeric SAS catalog storage width must be 0 or 3 through 8.");

    for (R_xlen_t i=0; i<count; i++) {
        double value = TYPEOF(labels) == REALSXP ? REAL(labels)[i] :
            INTEGER(labels)[i] == NA_INTEGER ? NA_REAL : INTEGER(labels)[i];
        char tag = 0;
        if (has_tags) {
            const char *definition = string_utf8_elt(tags, i);
            tag = definition[0];
            if (strlen(definition) > 1 ||
                    (tag && tag != '.' && tag != '_' && !(tag >= 'A' && tag <= 'Z')))
                Rf_error("Invalid SAS catalog label tag definition.");
            if (tag && R_FINITE(value)) {
                char code[64], mapped = 0;
                snprintf(code, sizeof(code), "%.17g", value);
                if (foreign_tag_from_code(ctx, column, code, &mapped, ctx->has_foreign_dictionary)) {
                    tag = mapped;
                } else if (!(tag >= 'A' && tag <= 'Z') || value != -91 - (tag - 'A')) {
                    Rf_error("Stale 'sas_metadata$label_tags'; label keys no longer match their missing tags.");
                }
            } else if ((!tag && ISNAN(value)) || (tag && !ISNAN(value))) {
                Rf_error("Stale 'sas_metadata$label_tags'; label keys no longer match their missing tags.");
            }
            if (tag && ISNAN(value)) {
                char actual = tagged_na_value(value);
                actual = actual ? toupper((unsigned char)actual) : '.';
                if (tag != actual)
                    Rf_error("Stale 'sas_metadata$label_tags'; missing label payload does not match its tag.");
            }
        } else if (ISNAN(value)) {
            tag = tagged_na_value(value);
            tag = tag ? toupper((unsigned char)tag) : '.';
            if (tag != '.' && tag != '_' && !(tag >= 'A' && tag <= 'Z'))
                Rf_error("Invalid SAS missing-value label tag.");
        } else if (!R_FINITE(value)) {
            Rf_error("SAS catalog label keys must be finite or missing values.");
        } else {
            char code[64];
            snprintf(code, sizeof(code), "%.17g", value);
            if (sas_catalog_is_missing_code(ctx, column, value))
                foreign_tag_from_code(ctx, column, code, &tag, 1);
        }
        if (!tag) {
            if (!R_FINITE(value))
                Rf_error("SAS catalog label keys must be finite or missing values.");
            value = adjustDatetimeFromR(DECLARED_SAS, x, value);
            if (!R_FINITE(value))
                Rf_error("SAS temporal value-label keys overflow their exported units.");
            if (width && width < 8) {
                uint64_t bits;
                double stored;
                memcpy(&bits, &value, 8);
                bits &= UINT64_MAX << ((8 - width) * 8);
                memcpy(&stored, &bits, 8);
                if (stored != value)
                    Rf_error("Reduced SAS numeric width cannot represent a value-label key exactly.");
            }
        }
        keys[i] = sas_catalog_key(value, tag);
        if (STRING_ELT(texts, i) == NA_STRING)
            Rf_error("SAS catalog label text must not be missing.");
        const char *text = string_utf8_elt(texts, i);
        if (strlen(text) > 32767)
            Rf_error("SAS catalog label text exceeds 32767 UTF-8 bytes.");

        if (shared) {
            int matched = 0;
            for (int j=0; j<set->value_labels_count; j++) {
                readstat_value_label_t *entry = &set->value_labels[j];
                if (keys[i] == sas_catalog_key(entry->double_key, entry->tag) &&
                        strlen(text) == entry->label_len &&
                        (entry->label_len == 0 || !memcmp(text, entry->label, entry->label_len))) {
                    matched = 1;
                    break;
                }
            }
            if (!matched)
                Rf_error("Conflicting value-label definitions for SAS format '%s'.", name);
        } else if (tag) {
            readstat_label_tagged_value(set, tag, text);
        } else {
            readstat_label_double_value(set, value, text);
        }
    }
    qsort(keys, count, sizeof(uint64_t), sas_catalog_key_compare);
    for (R_xlen_t i=1; i<count; i++) {
        if (keys[i] == keys[i-1])
            Rf_error("Duplicate exported value-label keys for SAS format '%s'.", name);
    }
}

static int sas_catalog_string_compare(const void *left, const void *right) {
    return strcmp(*(const char *const *)left, *(const char *const *)right);
}

static void sas_catalog_string_labels(WriterCtx *ctx, SEXP x, const char *name) {
    SEXP labels = Rf_getAttrib(x, Rf_install("labels"));
    SEXP texts = Rf_getAttrib(labels, R_NamesSymbol);
    R_xlen_t count = XLENGTH(labels);
    if (TYPEOF(x) != STRSXP || TYPEOF(labels) != STRSXP ||
            TYPEOF(texts) != STRSXP || XLENGTH(texts) != count)
        Rf_error("Character SAS catalog labels must be named character vectors.");
    if (ddiwr_metadata_field(x, DECLARED_SAS, "label_tags") != R_NilValue)
        Rf_error("'sas_metadata$label_tags' applies only to numeric SAS catalogs.");

    readstat_label_set_t *set = NULL;
    for (int i=0; i<ctx->writer->label_sets_count; i++) {
        if (!strcmp(ctx->writer->label_sets[i]->name, name)) {
            set = ctx->writer->label_sets[i];
            break;
        }
    }
    int shared = set != NULL;
    if (shared && (set->type != READSTAT_TYPE_STRING || set->value_labels_count != count))
        Rf_error("Conflicting value-label definitions for SAS format '%s'.", name);
    if (!shared)
        set = readstat_add_label_set(ctx->writer, READSTAT_TYPE_STRING, name);

    const char **keys = (const char **)R_alloc(count, sizeof(char *));
    for (R_xlen_t i=0; i<count; i++) {
        if (STRING_ELT(labels, i) == NA_STRING || STRING_ELT(texts, i) == NA_STRING)
            Rf_error("Character SAS catalog keys and label text must not be missing.");
        const char *value = string_utf8_elt(labels, i);
        size_t length = strlen(value);
        if (length > 32767)
            Rf_error("Character SAS catalog key exceeds 32767 UTF-8 bytes.");
        while (length && value[length-1] == ' ') length--;
        char *key = R_alloc(length + 1, 1);
        memcpy(key, value, length);
        key[length] = '\0';
        keys[i] = key;
        const char *text = string_utf8_elt(texts, i);
        if (strlen(text) > 32767)
            Rf_error("SAS catalog label text exceeds 32767 UTF-8 bytes.");

        if (shared) {
            int matched = 0;
            for (int j=0; j<set->value_labels_count; j++) {
                readstat_value_label_t *entry = &set->value_labels[j];
                if (length == entry->string_key_len &&
                        (!length || !memcmp(key, entry->string_key, length)) &&
                        strlen(text) == entry->label_len &&
                        (!entry->label_len || !memcmp(text, entry->label, entry->label_len))) {
                    matched = 1;
                    break;
                }
            }
            if (!matched)
                Rf_error("Conflicting value-label definitions for SAS format '%s'.", name);
        } else {
            readstat_label_string_value(set, key, text);
        }
    }
    qsort(keys, count, sizeof(char *), sas_catalog_string_compare);
    for (R_xlen_t i=1; i<count; i++) {
        if (!strcmp(keys[i], keys[i-1]))
            Rf_error("Duplicate exported value-label keys for SAS format '%s'.", name);
    }
}

/* R validates the rule table before staging either destination. Recheck the
 * primitive representation here because the native entry point is callable. */
static double sas_rule_number(const char *text, char *tag) {
    *tag = 0;
    if (!strcmp(text, "LOW")) return -DBL_MAX;
    if (!strcmp(text, "HIGH")) return DBL_MAX;
    if (text[0] == '.' && (!text[1] || (!text[2] &&
            (text[1] == '_' || (text[1] >= 'A' && text[1] <= 'Z'))))) {
        *tag = text[1] ? text[1] : '.';
        return NAN;
    }
    char *end; double value = strtod(text, &end);
    if (end == text || *end || !R_FINITE(value)) Rf_error("Invalid SAS catalog rule endpoint.");
    return value;
}

static void sas_catalog_rules(WriterCtx *ctx, SEXP column, const char *name) {
    VarType temporal = numTypeFromSEXP(column);
    if (temporal != DECLARED_DEFAULT) {
        SEXP informat = ddiwr_metadata_field(column, DECLARED_SAS, "informat");
        if (TYPEOF(informat) != STRSXP || XLENGTH(informat) != 1 ||
                numTypeFromMetadata(DECLARED_SAS, NULL, string_utf8_elt(informat, 0)) != temporal)
            Rf_error("Temporal SAS rules require an informat matching the column's date/time class.");
    }
    SEXP rules = ddiwr_metadata_field(column, DECLARED_SAS, "catalog_rules");
    const char *fields[] = { "start", "end", "label", "hlo", "sexcl", "eexcl", "default", "fuzz" };
    if (TYPEOF(rules) != VECSXP || XLENGTH(rules) != 8)
        Rf_error("SAS catalog rules must be a validated eight-column table.");
    SEXP values[8]; SEXP names = Rf_getAttrib(rules, R_NamesSymbol);
    if (TYPEOF(names) != STRSXP || XLENGTH(names) != 8) Rf_error("Invalid SAS catalog rule fields.");
    R_xlen_t count = XLENGTH(VECTOR_ELT(rules, 0));
    if (!count) Rf_error("SAS catalog rules must not be empty.");
    for (int j=0; j<8; j++) {
        if (strcmp(string_utf8_elt(names,j), fields[j])) Rf_error("Invalid SAS catalog rule fields.");
        values[j] = VECTOR_ELT(rules,j);
        if (TYPEOF(values[j]) != (j == 7 ? REALSXP : j == 6 ? INTSXP : STRSXP) || XLENGTH(values[j]) != count)
            Rf_error("Invalid SAS catalog rule column.");
    }
    for (int i=0; i<ctx->writer->label_sets_count; i++) {
        if (!strcmp(ctx->writer->label_sets[i]->name,name))
            Rf_error("Rule-based SAS formats need distinct format names per column.");
    }
    int string = TYPEOF(column) == STRSXP;
    readstat_label_set_t *set = readstat_add_label_set(ctx->writer,
            string ? READSTAT_TYPE_STRING : READSTAT_TYPE_DOUBLE, name);
    set->sas_default_width = INTEGER(values[6])[0];
    set->sas_fuzz = REAL(values[7])[0];
    if (set->sas_default_width < 1 || set->sas_default_width > 32767 ||
            !R_FINITE(set->sas_fuzz) || set->sas_fuzz < 0 || (string && set->sas_fuzz != 0))
        Rf_error("Invalid SAS catalog rule default width or fuzz.");
    for (R_xlen_t i=0; i<count; i++) {
        if (INTEGER(values[6])[i] != set->sas_default_width || REAL(values[7])[i] != set->sas_fuzz)
            Rf_error("SAS catalog rule default width and fuzz must be shared within a format.");
        for (int j=0; j<6; j++) if (STRING_ELT(values[j],i) == NA_STRING)
            Rf_error("SAS catalog rules must not contain missing strings.");
        const char *start = string_utf8_elt(values[0],i), *end = string_utf8_elt(values[1],i);
        const char *text = string_utf8_elt(values[2],i), *hlo = string_utf8_elt(values[3],i);
        unsigned flags = (strchr(hlo,'L') ? 1 : 0) | (strchr(hlo,'H') ? 2 : 0) |
            (strchr(hlo,'O') ? 16 : 0) | (!strcmp(string_utf8_elt(values[4],i),"Y") ? 4 : 0) |
            (!strcmp(string_utf8_elt(values[5],i),"Y") ? 8 : 0);
        if (strlen(text) > 32767) Rf_error("SAS catalog rule label exceeds 32767 bytes.");
        if (string) {
            if (strlen(start) > 32767 || strlen(end) > 32767 || strlen(start)+strlen(end) > 65500)
                Rf_error("Character SAS range endpoints exceed the native record capacity.");
            readstat_label_string_value(set,flags & 1 ? "" : start,text);
        } else {
            readstat_label_double_value(set,0,text);
        }
        readstat_value_label_t *entry = &set->value_labels[set->value_labels_count-1];
        entry->sas_is_rule = 1; entry->sas_rule_flags = flags;
        if (string) {
            entry->string_end_len = flags & 2 ? 0 : strlen(end);
            entry->string_end = strdup(flags & 2 ? "" : end);
            if (!entry->string_end) Rf_error("Could not allocate SAS rule endpoint.");
        } else if (!(flags & 16)) {
            entry->double_key = sas_rule_number(flags & 1 ? "LOW" : start, &entry->tag);
            entry->end_key = sas_rule_number(flags & 2 ? "HIGH" : end, &entry->end_tag);
        }
    }
}

typedef struct {
    WriterCtx writer;
    SEXP formats;
} SASCatalogCtx;

static SEXP sas_catalog_write_call(void *data) {
    SASCatalogCtx *catalog = data;
    WriterCtx *ctx = &catalog->writer;
    if (TYPEOF(catalog->formats) != STRSXP || XLENGTH(catalog->formats) != XLENGTH(ctx->data))
        Rf_error("SAS catalog format names must match the data columns.");
    init_missing_maps(ctx);
    init_foreign_dictionary(ctx, ctx->dictionary);
    init_foreign_missing_maps(ctx);
    /* SAS catalogs use the native 64-bit host layout; CEDA cannot translate them. */
    readstat_writer_set_file_format_is_64bit(ctx->writer, 1);
    for (R_xlen_t i=0; i<XLENGTH(ctx->data); i++) {
        if (STRING_ELT(catalog->formats, i) == NA_STRING)
            Rf_error("SAS catalog format names must not be missing.");
        const char *name = string_utf8_elt(catalog->formats, i);
        size_t length = strlen(name);
        SEXP column = VECTOR_ELT(ctx->data, i);
        int character_keys = TYPEOF(column) == STRSXP;
        const char *base = name + (name[0] == '$');
        size_t base_length = strlen(base);
        if (length && (length > 32 || base_length == 0 ||
                (name[0] == '$') != character_keys ||
                strspn(base, "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789_") != base_length ||
                isdigit((unsigned char)base[0]) || isdigit((unsigned char)base[base_length-1])))
            Rf_error("Invalid SAS catalog format name.");
        if (length) {
            if (ddiwr_metadata_field(column, DECLARED_SAS, "catalog_rules") != R_NilValue) {
                int shared = 0;
                for (R_xlen_t j=0; j<i; j++) {
                    if (!strcmp(name,string_utf8_elt(catalog->formats,j))) {
                        SEXP previous = VECTOR_ELT(ctx->data,j);
                        if (TYPEOF(previous) != TYPEOF(column) ||
                                !R_compute_identical(ddiwr_metadata_field(previous, DECLARED_SAS, "catalog_rules"),
                                    ddiwr_metadata_field(column, DECLARED_SAS, "catalog_rules"), 0))
                            Rf_error("Conflicting SAS catalog rules for format '%s'.",name);
                        shared = 1; break;
                    }
                }
                if (!shared) sas_catalog_rules(ctx, column, name);
            }
            else if (character_keys)
                sas_catalog_string_labels(ctx, column, name);
            else
                sas_catalog_labels(ctx, column, name, i);
        }
    }
    check_status(readstat_begin_writing_sas7bcat(ctx->writer, ctx), "Failed to initialize SAS catalog");
    ctx->out = fopen(ctx->path, "wb");
    if (!ctx->out)
        Rf_error("Failed to open '%s' for writing.", ctx->path);
    check_status(readstat_end_writing(ctx->writer), "Failed to write SAS catalog");
    FILE *out = ctx->out;
    ctx->out = NULL;
    if (fclose(out) != 0)
        Rf_error("Failed to close SAS catalog output '%s'.", ctx->path);
    return R_NilValue;
}

SEXP declared_write_sas_catalog_(SEXP data, SEXP path, SEXP formats, SEXP dictionary) {
    SASCatalogCtx catalog;
    writer_ctx_init(&catalog.writer, DECLARED_SAS7BCAT, data, path);
    catalog.writer.dictionary = dictionary;
    catalog.formats = formats;
    return R_ExecWithCleanup(sas_catalog_write_call, &catalog, writer_cleanup, &catalog.writer);
}

SEXP declared_write_sav_(SEXP data, SEXP path, SEXP compress) {
    WriterCtx ctx;
    writer_ctx_init(&ctx, DECLARED_SAV, data, path);
    set_compression(&ctx, CHAR(STRING_ELT(compress, 0)));
    set_file_label(&ctx, Rf_getAttrib(data, Rf_install("label")));
    ctx.dictionary = R_NilValue;
    return R_ExecWithCleanup(writer_write_call, &ctx, writer_cleanup, &ctx);
}

SEXP declared_write_dta_(SEXP data, SEXP path, SEXP version, SEXP label, SEXP strl_threshold, SEXP dictionary) {
    WriterCtx ctx;
    writer_ctx_init(&ctx, DECLARED_DTA, data, path);
    ctx.version = Rf_asInteger(version);
    ctx.strl_threshold = Rf_asInteger(strl_threshold);
    readstat_writer_set_file_format_version(ctx.writer, ctx.version);
    set_file_label(&ctx, label);
    ctx.dictionary = dictionary;
    return R_ExecWithCleanup(writer_write_call, &ctx, writer_cleanup, &ctx);
}

SEXP declared_write_sas_(SEXP data, SEXP path, SEXP dictionary) {
    WriterCtx ctx;
    writer_ctx_init(&ctx, DECLARED_SAS7BDAT, data, path);
    set_file_label(&ctx, Rf_getAttrib(data, Rf_install("label")));
    ctx.dictionary = dictionary;
    return R_ExecWithCleanup(writer_write_call, &ctx, writer_cleanup, &ctx);
}

SEXP declared_write_xpt_(SEXP data, SEXP path, SEXP version, SEXP name, SEXP label, SEXP dictionary) {
    WriterCtx ctx;
    writer_ctx_init(&ctx, DECLARED_XPT, data, path);
    ctx.version = Rf_asInteger(version);
    readstat_writer_set_file_format_version(ctx.writer, ctx.version);
    if (TYPEOF(name) == STRSXP && Rf_length(name) > 0) {
        readstat_writer_set_table_name(ctx.writer, string_utf8_elt(name, 0));
    }
    set_file_label(&ctx, label);
    ctx.dictionary = dictionary;
    return R_ExecWithCleanup(writer_write_call, &ctx, writer_cleanup, &ctx);
}
