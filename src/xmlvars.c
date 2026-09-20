#include <R.h>
#include <Rinternals.h>
#include <R_ext/Utils.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdarg.h>
#include <math.h>
#include <limits.h>

#include "variable_analysis.h"
#include "variable_threads.h"


typedef struct {
    char *buf;
    size_t len;
    size_t cap;
} ddiwr_strbuf;

typedef struct {
    SEXP x;
    SEXP classes_attr;
    SEXP label;
    SEXP measurement;
    SEXP labels;
    SEXP levels;
    SEXP na_values;
    SEXP na_range;
    SEXP xmlang;
    SEXP id;
    int factor_fallback;
    int include_formats;
    int is_date;
    char format_spss[64];
    char format_stata[64];
} xmlmeta_result;

static SEXP ddiwr_sym_labels = NULL;
static SEXP ddiwr_sym_label = NULL;
static SEXP ddiwr_sym_measurement = NULL;
static SEXP ddiwr_sym_na_values = NULL;
static SEXP ddiwr_sym_na_range = NULL;
static SEXP ddiwr_sym_xmlang = NULL;
static SEXP ddiwr_sym_id = NULL;

static void ddiwr_init_symbols(void) {
    if (ddiwr_sym_labels == NULL) {
        ddiwr_sym_labels = Rf_install("labels");
        ddiwr_sym_label = Rf_install("label");
        ddiwr_sym_measurement = Rf_install("measurement");
        ddiwr_sym_na_values = Rf_install("na_values");
        ddiwr_sym_na_range = Rf_install("na_range");
        ddiwr_sym_xmlang = Rf_install("xmlang");
        ddiwr_sym_id = Rf_install("ID");
    }
}


// Forward declarations for R-dependent functions used in extraction
static int sexp_as_double(SEXP x, R_xlen_t i, double *out);
static int label_is_missing(SEXP labels, R_xlen_t j, SEXP na_values, SEXP na_range);
static SEXP getListElement(SEXP list, const char *name);
static int class_has(SEXP classes, const char *target);

static int label_matches_any_name(SEXP labels, R_xlen_t index, SEXP label_names) {
    R_xlen_t name_index = 0;

    if (TYPEOF(label_names) != STRSXP || XLENGTH(label_names) == 0) {
        return 0;
    }

    if (TYPEOF(labels) == STRSXP) {
        SEXP value = STRING_ELT(labels, index);

        if (value == NA_STRING) {
            return 0;
        }

        for (name_index = 0; name_index < XLENGTH(label_names); name_index++) {
            SEXP name = STRING_ELT(label_names, name_index);

            if (name != NA_STRING && strcmp(CHAR(value), CHAR(name)) == 0) {
                return 1;
            }
        }

        return 0;
    }

    {
        double value = 0.0;
        char buffer[128];

        if (!sexp_as_double(labels, index, &value)) {
            return 0;
        }

        if (TYPEOF(labels) == LGLSXP) {
            snprintf(buffer, sizeof(buffer), "%s", value == 0.0 ? "FALSE" : "TRUE");
        }
        else {
            snprintf(buffer, sizeof(buffer), "%.15g", value);
        }

        for (name_index = 0; name_index < XLENGTH(label_names); name_index++) {
            SEXP name = STRING_ELT(label_names, name_index);

            if (name != NA_STRING && strcmp(buffer, CHAR(name)) == 0) {
                return 1;
            }
        }
    }

    return 0;
}

static void extract_variable_data(
    SEXP data, SEXP variables, SEXP dates, R_xlen_t i, 
    R_xlen_t *cat_offsets, R_xlen_t **cat_label_idx_arr, int *cat_counts_arr,
    double *cat_freq_out, int include_projection, CVariableData *job
) {
    SEXP x = VECTOR_ELT(data, i);
    SEXP metadata = VECTOR_ELT(variables, i);
    SEXP labels = getListElement(metadata, "labels");
    SEXP na_values = getListElement(metadata, "na_values");
    SEXP na_range = getListElement(metadata, "na_range");
    SEXP type = getListElement(metadata, "type");

    job->index = (int)i;
    job->type = TYPEOF(x);
    job->len = XLENGTH(x);
    job->is_numericish = (TYPEOF(x) == REALSXP || TYPEOF(x) == INTSXP || TYPEOF(x) == LGLSXP || TYPEOF(x) == STRSXP);
    job->date_var = LOGICAL(dates)[i] == TRUE;
    job->has_labels = (labels != R_NilValue);
    job->cat_count = cat_counts_arr[i];
    job->classification_label_count = 0;
    job->classification_label_dvals = NULL;
    job->classification_label_svals = NULL;
    job->classification_label_missing = NULL;
    job->classification_labels_numeric = 0;
    job->source_is_numeric = TYPEOF(x) == REALSXP || TYPEOF(x) == INTSXP;
    job->source_numeric_candidate = job->source_is_numeric || TYPEOF(x) == STRSXP;
    job->include_projection = include_projection;

    {
        SEXP classes = getListElement(metadata, "classes");

        if (class_has(classes, "factor") || TYPEOF(x) == LGLSXP) {
            job->source_numeric_candidate = 0;
            job->source_is_numeric = 0;
        }
    }

    job->has_type_num = 0;
    if (type != R_NilValue && TYPEOF(type) == STRSXP && XLENGTH(type) > 0) {
        const char *ct = CHAR(STRING_ELT(type, 0));
        if (strstr(ct, "num") != NULL) {
            job->has_type_num = 1;
        }
    }

    // Assign data pointers
    job->real_data = NULL;
    job->int_data = NULL;
    job->lgl_data = NULL;
    job->str_data = NULL;

    if (TYPEOF(x) == REALSXP) {
        job->real_data = REAL(x);
    } else if (TYPEOF(x) == INTSXP) {
        job->int_data = INTEGER(x);
    } else if (TYPEOF(x) == LGLSXP) {
        job->lgl_data = LOGICAL(x);
    } else if (TYPEOF(x) == STRSXP) {
        job->str_data = (const char **)malloc((size_t)job->len * sizeof(char *));
        for (R_xlen_t j = 0; j < job->len; j++) {
            if (STRING_ELT(x, j) == NA_STRING) {
                job->str_data[j] = NULL;
            } else {
                job->str_data[j] = CHAR(STRING_ELT(x, j));
            }
        }
    }

    // Extract missing values
    job->num_na_values_n = 0;
    job->has_num_na_range = 0;
    job->str_na_values_n = 0;

    if (na_values != R_NilValue && XLENGTH(na_values) > 0) {
        if (TYPEOF(x) == STRSXP && TYPEOF(na_values) == STRSXP) {
            job->str_na_values_n = (int)XLENGTH(na_values);
            if (job->str_na_values_n > 3) job->str_na_values_n = 3;
            for (int k = 0; k < job->str_na_values_n; k++) {
                if (STRING_ELT(na_values, k) == NA_STRING) {
                    job->str_na_values[k] = NULL;
                } else {
                    job->str_na_values[k] = CHAR(STRING_ELT(na_values, k));
                }
            }
        } else {
            int n_vals = (int)XLENGTH(na_values);
            if (n_vals > 3) n_vals = 3;
            for (int k = 0; k < n_vals; k++) {
                double val = 0.0;
                if (sexp_as_double(na_values, k, &val)) {
                    job->num_na_values[job->num_na_values_n++] = val;
                }
            }
        }
    }

    if (na_range != R_NilValue && XLENGTH(na_range) == 2) {
        job->has_num_na_range = 1;
        job->num_na_range[0] = REAL(na_range)[0];
        job->num_na_range[1] = REAL(na_range)[1];
    }

    // Extract category labels
    job->cat_label_dvals = NULL;
    job->cat_label_svals = NULL;
    job->cat_label_idx = NULL;
    job->cat_missing = NULL;
    job->cat_freq = NULL;

    if (job->has_labels && job->cat_count > 0) {
        job->cat_label_idx = (ptrdiff_t *)malloc((size_t)job->cat_count * sizeof(ptrdiff_t));
        for (int k = 0; k < job->cat_count; k++) {
            job->cat_label_idx[k] = (ptrdiff_t)cat_label_idx_arr[i][k];
        }

        if (TYPEOF(labels) == STRSXP) {
            job->cat_label_svals = (const char **)malloc((size_t)job->cat_count * sizeof(char *));
            for (int k = 0; k < job->cat_count; k++) {
                R_xlen_t idx = job->cat_label_idx[k];
                if (STRING_ELT(labels, idx) == NA_STRING) {
                    job->cat_label_svals[k] = NULL;
                } else {
                    job->cat_label_svals[k] = CHAR(STRING_ELT(labels, idx));
                }
            }
        } else {
            job->cat_label_dvals = (double *)malloc((size_t)job->cat_count * sizeof(double));
            for (int k = 0; k < job->cat_count; k++) {
                R_xlen_t idx = job->cat_label_idx[k];
                double val = 0.0;
                if (sexp_as_double(labels, idx, &val)) {
                    job->cat_label_dvals[k] = val;
                } else {
                    job->cat_label_dvals[k] = NA_REAL;
                }
            }
        }

        job->cat_missing = (int *)malloc((size_t)job->cat_count * sizeof(int));
        for (int k = 0; k < job->cat_count; k++) {
            R_xlen_t idx = job->cat_label_idx[k];
            job->cat_missing[k] = label_is_missing(labels, idx, na_values, na_range);
        }

        job->cat_freq = &cat_freq_out[cat_offsets[i]];
        for (int k = 0; k < job->cat_count; k++) {
            job->cat_freq[k] = 0.0;
        }
    }

    if (include_projection && labels != R_NilValue && XLENGTH(labels) > 0) {
        SEXP label_names = getAttrib(labels, R_NamesSymbol);
        int retained = 0;
        int labels_numeric = TYPEOF(labels) == REALSXP || TYPEOF(labels) == INTSXP ||
            TYPEOF(labels) == STRSXP;
        int labels_have_observed = 0;

        for (R_xlen_t k = 0; k < XLENGTH(labels); k++) {
            if (!label_matches_any_name(labels, k, label_names)) {
                retained++;
            }
        }

        if (retained > 0) {
            int destination = 0;

            job->classification_label_count = retained;
            job->classification_label_dvals = (double *)malloc(
                (size_t)retained * sizeof(double)
            );
            job->classification_label_svals = (const char **)calloc(
                (size_t)retained, sizeof(char *)
            );
            job->classification_label_missing = (int *)malloc(
                (size_t)retained * sizeof(int)
            );

            if (job->classification_label_dvals == NULL ||
                job->classification_label_svals == NULL ||
                job->classification_label_missing == NULL) {
                job->error = 1;
                return;
            }

            for (R_xlen_t k = 0; k < XLENGTH(labels); k++) {
                double numeric_value = NA_REAL;

                if (label_matches_any_name(labels, k, label_names)) {
                    continue;
                }

                if (TYPEOF(labels) == STRSXP) {
                    SEXP value = STRING_ELT(labels, k);

                    if (value != NA_STRING) {
                        char *endptr = NULL;
                        const char *text = CHAR(value);

                        job->classification_label_svals[destination] = text;
                        numeric_value = strtod(text, &endptr);
                        if (endptr == text || *endptr != '\0') {
                            numeric_value = NA_REAL;
                            labels_numeric = 0;
                        }
                        else {
                            labels_have_observed = 1;
                        }
                    }
                }
                else if (sexp_as_double(labels, k, &numeric_value)) {
                    labels_have_observed = 1;
                }

                job->classification_label_dvals[destination] = numeric_value;
                job->classification_label_missing[destination] = label_is_missing(
                    labels, k, na_values, na_range
                );
                destination++;
            }

            job->classification_labels_numeric = labels_numeric && labels_have_observed;
        }
    }
}

static SEXP getListElement(SEXP list, const char *name) {
    SEXP names = getAttrib(list, R_NamesSymbol);
    R_xlen_t i = 0;
    if (TYPEOF(list) != VECSXP || TYPEOF(names) != STRSXP) {
        return R_NilValue;
    }
    for (i = 0; i < XLENGTH(list); i++) {
        if (strcmp(CHAR(STRING_ELT(names, i)), name) == 0) {
            return VECTOR_ELT(list, i);
        }
    }
    return R_NilValue;
}

static int class_has(SEXP classes, const char *target) {
    R_xlen_t i = 0;

    if (TYPEOF(classes) != STRSXP) {
        return 0;
    }

    for (i = 0; i < XLENGTH(classes); i++) {
        if (STRING_ELT(classes, i) != NA_STRING &&
            strcmp(CHAR(STRING_ELT(classes, i)), target) == 0) {
            return 1;
        }
    }

    return 0;
}


static int parse_string_double(SEXP x, R_xlen_t i, double *out) {
    const char *s = NULL;
    char *end = NULL;
    double val = 0.0;

    if (STRING_ELT(x, i) == NA_STRING) {
        return 0;
    }

    s = CHAR(STRING_ELT(x, i));
    val = strtod(s, &end);
    if (end == s || *end != '\0') {
        return 0;
    }

    *out = val;
    return 1;
}

static int vector_possible_numeric(SEXP x) {
    R_xlen_t i = 0;
    double tmp = 0.0;

    if (x == R_NilValue) {
        return 1;
    }

    switch(TYPEOF(x)) {
        case REALSXP:
        case INTSXP:
        case LGLSXP:
            return 1;
        case STRSXP:
            for (i = 0; i < XLENGTH(x); i++) {
                if (STRING_ELT(x, i) == NA_STRING) {
                    continue;
                }
                if (!parse_string_double(x, i, &tmp)) {
                    return 0;
                }
            }
            return 1;
        default:
            return 0;
    }
}

static int sexp_as_double(SEXP x, R_xlen_t i, double *out) {
    switch(TYPEOF(x)) {
        case REALSXP:
            *out = REAL(x)[i];
            return !ISNAN(*out);
        case INTSXP:
            if (INTEGER(x)[i] == NA_INTEGER) {
                return 0;
            }
            *out = (double)INTEGER(x)[i];
            return 1;
        case LGLSXP:
            if (LOGICAL(x)[i] == NA_LOGICAL) {
                return 0;
            }
            *out = (double)LOGICAL(x)[i];
            return 1;
        case STRSXP:
            return parse_string_double(x, i, out);
        default:
            return 0;
    }
}

static int string_width_sexp(SEXP x, R_xlen_t i) {
    if (TYPEOF(x) != STRSXP || STRING_ELT(x, i) == NA_STRING) {
        return 0;
    }

    return (int)strlen(CHAR(STRING_ELT(x, i)));
}

static int display_width_sexp(SEXP x, R_xlen_t i) {
    char buf[128];

    switch(TYPEOF(x)) {
        case REALSXP:
            if (ISNAN(REAL(x)[i])) {
                return 0;
            }
            snprintf(buf, sizeof(buf), "%.15g", REAL(x)[i]);
            return (int)strlen(buf);
        case INTSXP:
            if (INTEGER(x)[i] == NA_INTEGER) {
                return 0;
            }
            snprintf(buf, sizeof(buf), "%d", INTEGER(x)[i]);
            return (int)strlen(buf);
        case LGLSXP:
            if (LOGICAL(x)[i] == NA_LOGICAL) {
                return 0;
            }
            return LOGICAL(x)[i] ? 4 : 5;
        case STRSXP:
            return string_width_sexp(x, i);
        default:
            return 0;
    }
}

static void infer_formats(SEXP x, SEXP classes, SEXP labels, char *spss, size_t spss_sz, char *stata, size_t stata_sz, int *is_date) {
    int pN = 0;
    int allnax = 1;
    int nullabels = labels == R_NilValue;
    int decimals = 0;
    int numeric_width = 1;
    int maxvarchar = 0;
    R_xlen_t i = 0;

    *is_date = 0;

    if (class_has(classes, "POSIXct")) {
        snprintf(spss, spss_sz, "DATETIME");
        snprintf(stata, stata_sz, "%%tc");
        return;
    }

    if (class_has(classes, "Date")) {
        *is_date = 1;
        spss[0] = '\0';
        stata[0] = '\0';
        return;
    }

    if (class_has(classes, "hms")) {
        snprintf(spss, spss_sz, "TIME");
        snprintf(stata, stata_sz, "%%tc");
        return;
    }

    pN = (TYPEOF(x) != STRSXP) && vector_possible_numeric(x);
    if (!nullabels) {
        pN = pN && vector_possible_numeric(labels);
    }

    for (i = 0; i < XLENGTH(x); i++) {
        if (TYPEOF(x) == STRSXP) {
            if (STRING_ELT(x, i) != NA_STRING) {
                allnax = 0;
                break;
            }
        }
        else {
            double tmp = 0.0;
            if (sexp_as_double(x, i, &tmp)) {
                allnax = 0;
                break;
            }
        }
    }

    if (pN && !allnax) {
        for (i = 0; i < XLENGTH(x); i++) {
            double val = 0.0;
            int width = 0;

            if (!sexp_as_double(x, i, &val)) {
                continue;
            }

            width = display_width_sexp(x, i);
            if (width > numeric_width) {
                numeric_width = width;
            }

            if (decimals < 3) {
                int d = ddiwr_decimal_count(val);
                if (d > decimals) {
                    decimals = d > 3 ? 3 : d;
                }
            }
        }
    }

    if (!pN && !allnax) {
        for (i = 0; i < XLENGTH(x); i++) {
            int width = string_width_sexp(x, i);
            if (width > maxvarchar) {
                maxvarchar = width;
            }
        }
    }

    if (!nullabels && !pN) {
        for (i = 0; i < XLENGTH(labels); i++) {
            int width = string_width_sexp(labels, i);
            if (width > maxvarchar) {
                maxvarchar = width;
            }
        }
    }

    if (pN) {
        snprintf(spss, spss_sz, "F%d.%d", numeric_width, decimals);
        snprintf(stata, stata_sz, "%%%d.%dg", numeric_width, decimals);
    }
    else {
        int width = maxvarchar > 0 ? maxvarchar : 1;
        snprintf(spss, spss_sz, "A%d", width);
        snprintf(stata, stata_sz, "%%%ds", width);
    }
}

static SEXP sanitize_na_values(SEXP na_values) {
    SEXP out = R_NilValue;
    R_xlen_t i = 0;
    R_xlen_t n = 0;

    if (na_values == R_NilValue) {
        return R_NilValue;
    }

    switch(TYPEOF(na_values)) {
        case REALSXP:
            for (i = 0; i < XLENGTH(na_values); i++) {
                if (!ISNAN(REAL(na_values)[i])) {
                    n++;
                }
            }
            if (n == 0) {
                return R_NilValue;
            }
            PROTECT(out = allocVector(REALSXP, n));
            n = 0;
            for (i = 0; i < XLENGTH(na_values); i++) {
                if (!ISNAN(REAL(na_values)[i])) {
                    REAL(out)[n++] = REAL(na_values)[i];
                }
            }
            UNPROTECT(1);
            return out;
        case INTSXP:
        case LGLSXP:
            for (i = 0; i < XLENGTH(na_values); i++) {
                int val = INTEGER(na_values)[i];
                if (val != NA_INTEGER) {
                    n++;
                }
            }
            if (n == 0) {
                return R_NilValue;
            }
            PROTECT(out = allocVector(TYPEOF(na_values), n));
            n = 0;
            for (i = 0; i < XLENGTH(na_values); i++) {
                int val = INTEGER(na_values)[i];
                if (val != NA_INTEGER) {
                    INTEGER(out)[n++] = val;
                }
            }
            UNPROTECT(1);
            return out;
        case STRSXP:
            for (i = 0; i < XLENGTH(na_values); i++) {
                if (STRING_ELT(na_values, i) != NA_STRING) {
                    n++;
                }
            }
            if (n == 0) {
                return R_NilValue;
            }
            PROTECT(out = allocVector(STRSXP, n));
            n = 0;
            for (i = 0; i < XLENGTH(na_values); i++) {
                if (STRING_ELT(na_values, i) != NA_STRING) {
                    SET_STRING_ELT(out, n++, STRING_ELT(na_values, i));
                }
            }
            UNPROTECT(1);
            return out;
        default:
            return na_values;
    }
}

static void xmlmeta_process_variable(SEXP data, xmlmeta_result *results, R_xlen_t i, int include_formats) {
    SEXP x = VECTOR_ELT(data, i);
    SEXP classes = getAttrib(x, R_ClassSymbol);
    SEXP labels = getAttrib(x, ddiwr_sym_labels);
    SEXP levels = getAttrib(x, R_LevelsSymbol);

    results[i].x = x;
    results[i].classes_attr = classes;
    results[i].label = getAttrib(x, ddiwr_sym_label);
    results[i].measurement = getAttrib(x, ddiwr_sym_measurement);
    results[i].labels = labels;
    results[i].levels = R_NilValue;
    results[i].na_values = getAttrib(x, ddiwr_sym_na_values);
    results[i].na_range = getAttrib(x, ddiwr_sym_na_range);
    results[i].xmlang = getAttrib(x, ddiwr_sym_xmlang);
    results[i].id = getAttrib(x, ddiwr_sym_id);
    results[i].factor_fallback = 0;
    results[i].include_formats = include_formats;
    results[i].is_date = 0;
    results[i].format_spss[0] = '\0';
    results[i].format_stata[0] = '\0';

    if (labels == R_NilValue && class_has(classes, "factor") && TYPEOF(levels) == STRSXP) {
        results[i].factor_fallback = 1;
        results[i].levels = levels;
    }

    if (include_formats) {
        infer_formats(
            x,
            classes,
            labels,
            results[i].format_spss,
            sizeof(results[i].format_spss),
            results[i].format_stata,
            sizeof(results[i].format_stata),
            &results[i].is_date
        );
    }
}



static int char_equal_sexp(SEXP x, R_xlen_t i, SEXP labels, R_xlen_t j) {
    if (TYPEOF(x) != STRSXP || TYPEOF(labels) != STRSXP) {
        return 0;
    }
    if (STRING_ELT(x, i) == NA_STRING || STRING_ELT(labels, j) == NA_STRING) {
        return 0;
    }
    return strcmp(CHAR(STRING_ELT(x, i)), CHAR(STRING_ELT(labels, j))) == 0;
}

static int value_in_na_values(SEXP x, R_xlen_t i, SEXP na_values) {
    R_xlen_t j = 0;
    double xnum = 0.0;
    double nnum = 0.0;

    if (na_values == R_NilValue || XLENGTH(na_values) == 0) {
        return 0;
    }

    if (TYPEOF(x) == STRSXP && TYPEOF(na_values) == STRSXP) {
        for (j = 0; j < XLENGTH(na_values); j++) {
            if (char_equal_sexp(x, i, na_values, j)) {
                return 1;
            }
        }
        return 0;
    }

    if (!sexp_as_double(x, i, &xnum)) {
        return 0;
    }

    for (j = 0; j < XLENGTH(na_values); j++) {
        if (sexp_as_double(na_values, j, &nnum) && xnum == nnum) {
            return 1;
        }
    }

    return 0;
}

static int value_in_na_range(SEXP x, R_xlen_t i, SEXP na_range) {
    double xnum = 0.0;
    double lo = 0.0;
    double hi = 0.0;

    if (na_range == R_NilValue || XLENGTH(na_range) < 2) {
        return 0;
    }

    if (!sexp_as_double(x, i, &xnum)) {
        return 0;
    }

    lo = REAL(na_range)[0];
    hi = REAL(na_range)[1];

    if (R_NegInf == lo) {
        return xnum <= hi;
    }
    if (R_PosInf == hi) {
        return xnum >= lo;
    }
    return xnum >= lo && xnum <= hi;
}

static int label_is_missing(SEXP labels, R_xlen_t j, SEXP na_values, SEXP na_range) {
    if (labels == R_NilValue || XLENGTH(labels) <= j) {
        return 0;
    }

    if (TYPEOF(labels) == STRSXP && TYPEOF(na_values) == STRSXP) {
        R_xlen_t k = 0;
        for (k = 0; k < XLENGTH(na_values); k++) {
            if (char_equal_sexp(labels, j, na_values, k)) {
                return 1;
            }
        }
        return 0;
    }

    if (value_in_na_values(labels, j, na_values)) {
        return 1;
    }
    return value_in_na_range(labels, j, na_range);
}

static int value_matches_label(SEXP x, R_xlen_t i, SEXP labels, R_xlen_t j) {
    double xnum = 0.0;
    double lnum = 0.0;

    if (TYPEOF(x) == STRSXP && TYPEOF(labels) == STRSXP) {
        return char_equal_sexp(x, i, labels, j);
    }

    if (!sexp_as_double(x, i, &xnum) || !sexp_as_double(labels, j, &lnum)) {
        return 0;
    }

    return xnum == lnum;
}




/* Called only by the R thread, before extracting any worker inputs. */
static int ddiwr_requested_variable_threads(void) {
    SEXP option = Rf_GetOption1(Rf_install("DDIwR.variable_threads"));
    if (option == R_NilValue) {
        return 0;
    }
    if (!Rf_isNumeric(option) || XLENGTH(option) != 1) {
        Rf_error("Option 'DDIwR.variable_threads' must be one integer from 0 to 256.");
    }
    double value = Rf_asReal(option);
    if (!R_FINITE(value) || value < 0 || value > 256 || floor(value) != value) {
        Rf_error("Option 'DDIwR.variable_threads' must be one integer from 0 to 256.");
    }
    return (int)value;
}

SEXP collect_datadscr_stats(SEXP data, SEXP variables, SEXP dates, SEXP include_projection) {
    int requested_threads = ddiwr_requested_variable_threads();
    R_xlen_t n = 0;
    R_xlen_t i = 0;
    R_xlen_t cat_total = 0;
    SEXP out = R_NilValue;
    SEXP names = R_NilValue;
    SEXP var_dcml = R_NilValue;
    SEXP var_width = R_NilValue;
    SEXP range_units = R_NilValue;
    SEXP val_min = R_NilValue;
    SEXP val_max = R_NilValue;
    SEXP stat_min = R_NilValue;
    SEXP stat_max = R_NilValue;
    SEXP stat_mean = R_NilValue;
    SEXP stat_medn = R_NilValue;
    SEXP stat_stdev = R_NilValue;
    SEXP sum_valid = R_NilValue;
    SEXP sum_invalid = R_NilValue;
    SEXP cat_counts = R_NilValue;
    SEXP cat_values = R_NilValue;
    SEXP cat_labels = R_NilValue;
    SEXP cat_missing = R_NilValue;
    SEXP cat_freq = R_NilValue;
    SEXP variable_type = R_NilValue;
    SEXP weight_numeric_compatible = R_NilValue;
    SEXP weight_has_labels = R_NilValue;
    SEXP weight_has_observed = R_NilValue;
    SEXP weight_has_negative = R_NilValue;
    R_xlen_t *cat_offsets = NULL;
    R_xlen_t **cat_label_idx_arr = NULL;
    int *cat_counts_arr = NULL;
    int do_projection = 0;

    if (!Rf_isNewList(data) || !Rf_isNewList(variables)) {
        Rf_error("Arguments 'data' and 'variables' must be lists.");
    }
    if (!Rf_isLogical(include_projection) || XLENGTH(include_projection) != 1 ||
        LOGICAL(include_projection)[0] == NA_LOGICAL) {
        Rf_error("Argument 'include_projection' must be TRUE or FALSE.");
    }
    do_projection = LOGICAL(include_projection)[0] == TRUE;

    n = XLENGTH(data);
    if (XLENGTH(variables) != n || !Rf_isLogical(dates) || XLENGTH(dates) != n) {
        Rf_error("Arguments 'variables' and 'dates' must have same length as 'data'.");
    }

    for (i = 0; i < n; i++) {
        SEXP metadata = VECTOR_ELT(variables, i);
        SEXP labels = getListElement(metadata, "labels");
        SEXP label_names = (labels == R_NilValue) ? R_NilValue : getAttrib(labels, R_NamesSymbol);
        R_xlen_t j = 0;
        if (labels == R_NilValue || TYPEOF(label_names) != STRSXP) {
            continue;
        }
        for (j = 0; j < XLENGTH(labels); j++) {
            if (STRING_ELT(label_names, j) != NA_STRING && strlen(CHAR(STRING_ELT(label_names, j))) > 0) {
                cat_total++;
            }
        }
    }

    PROTECT(out = allocVector(VECSXP, 22));
    PROTECT(names = allocVector(STRSXP, 22));
    PROTECT(var_dcml = allocVector(REALSXP, n));
    PROTECT(var_width = allocVector(REALSXP, n));
    PROTECT(range_units = allocVector(STRSXP, n));
    PROTECT(val_min = allocVector(REALSXP, n));
    PROTECT(val_max = allocVector(REALSXP, n));
    PROTECT(stat_min = allocVector(REALSXP, n));
    PROTECT(stat_max = allocVector(REALSXP, n));
    PROTECT(stat_mean = allocVector(REALSXP, n));
    PROTECT(stat_medn = allocVector(REALSXP, n));
    PROTECT(stat_stdev = allocVector(REALSXP, n));
    PROTECT(sum_valid = allocVector(REALSXP, n));
    PROTECT(sum_invalid = allocVector(REALSXP, n));
    PROTECT(cat_counts = allocVector(INTSXP, n));
    PROTECT(cat_values = allocVector(STRSXP, cat_total));
    PROTECT(cat_labels = allocVector(STRSXP, cat_total));
    PROTECT(cat_missing = allocVector(LGLSXP, cat_total));
    PROTECT(cat_freq = allocVector(REALSXP, cat_total));
    PROTECT(variable_type = allocVector(STRSXP, n));
    PROTECT(weight_numeric_compatible = allocVector(LGLSXP, n));
    PROTECT(weight_has_labels = allocVector(LGLSXP, n));
    PROTECT(weight_has_observed = allocVector(LGLSXP, n));
    PROTECT(weight_has_negative = allocVector(LGLSXP, n));

    cat_offsets = (R_xlen_t *)calloc((size_t)n, sizeof(R_xlen_t));
    cat_label_idx_arr = (R_xlen_t **)calloc((size_t)n, sizeof(R_xlen_t *));
    cat_counts_arr = (int *)calloc((size_t)n, sizeof(int));
    if (cat_offsets == NULL || cat_label_idx_arr == NULL || cat_counts_arr == NULL) {
        free(cat_offsets);
        free(cat_label_idx_arr);
        free(cat_counts_arr);
        UNPROTECT(24);
        Rf_error("Failed to allocate category metadata buffers.");
    }

    for (i = 0; i < n; i++) {
        REAL(var_dcml)[i] = NA_REAL;
        REAL(var_width)[i] = NA_REAL;
        SET_STRING_ELT(range_units, i, mkChar("REAL"));
        REAL(val_min)[i] = NA_REAL;
        REAL(val_max)[i] = NA_REAL;
        REAL(stat_min)[i] = NA_REAL;
        REAL(stat_max)[i] = NA_REAL;
        REAL(stat_mean)[i] = NA_REAL;
        REAL(stat_medn)[i] = NA_REAL;
        REAL(stat_stdev)[i] = NA_REAL;
        REAL(sum_valid)[i] = NA_REAL;
        REAL(sum_invalid)[i] = NA_REAL;
        INTEGER(cat_counts)[i] = 0;
    }

    for (i = 0; i < n; i++) {
        SEXP metadata = VECTOR_ELT(variables, i);
        SEXP labels = getListElement(metadata, "labels");
        SEXP label_names = (labels == R_NilValue) ? R_NilValue : getAttrib(labels, R_NamesSymbol);
        SEXP na_values = getListElement(metadata, "na_values");
        SEXP na_range = getListElement(metadata, "na_range");
        R_xlen_t j = 0;
        int has_labels = labels != R_NilValue && TYPEOF(label_names) == STRSXP;
        int cat_count = 0;

        cat_offsets[i] = (i == 0) ? 0 : (cat_offsets[i - 1] + (R_xlen_t)cat_counts_arr[i - 1]);

        if (has_labels) {
            R_xlen_t *cat_label_idx = (R_xlen_t *)calloc((size_t)XLENGTH(labels), sizeof(R_xlen_t));
            if (cat_label_idx == NULL) {
                R_xlen_t k = 0;
                for (k = 0; k < i; k++) {
                    free(cat_label_idx_arr[k]);
                }
                free(cat_offsets);
                free(cat_label_idx_arr);
                free(cat_counts_arr);
                UNPROTECT(24);
                Rf_error("Failed to allocate category index buffer.");
            }
            cat_label_idx_arr[i] = cat_label_idx;
            for (j = 0; j < XLENGTH(labels); j++) {
                if (STRING_ELT(label_names, j) != NA_STRING && strlen(CHAR(STRING_ELT(label_names, j))) > 0) {
                    R_xlen_t pos = cat_offsets[i] + cat_count;

                    cat_label_idx[cat_count] = j;
                    if (TYPEOF(labels) == STRSXP) {
                        if (STRING_ELT(labels, j) == NA_STRING) {
                            SET_STRING_ELT(cat_values, pos, NA_STRING);
                        } else {
                            SET_STRING_ELT(cat_values, pos, STRING_ELT(labels, j));
                        }
                    } else {
                        char buf[128];
                        double lnum = 0.0;
                        if (sexp_as_double(labels, j, &lnum)) {
                            snprintf(buf, sizeof(buf), "%.15g", lnum);
                            SET_STRING_ELT(cat_values, pos, mkChar(buf));
                        } else {
                            SET_STRING_ELT(cat_values, pos, NA_STRING);
                        }
                    }
                    SET_STRING_ELT(cat_labels, pos, STRING_ELT(label_names, j));
                    LOGICAL(cat_missing)[pos] = label_is_missing(labels, j, na_values, na_range);
                    REAL(cat_freq)[pos] = 0.0;
                    cat_count++;
                }
            }
            INTEGER(cat_counts)[i] = cat_count;
            cat_counts_arr[i] = cat_count;
        }
    }

    CVariableData *jobs = (CVariableData *)calloc((size_t)n, sizeof(CVariableData));
    if (jobs == NULL) {
        for (i = 0; i < n; i++) {
            free(cat_label_idx_arr[i]);
        }
        free(cat_offsets);
        free(cat_label_idx_arr);
        free(cat_counts_arr);
        UNPROTECT(24);
        Rf_error("Failed to allocate C statistics jobs.");
    }

    for (i = 0; i < n; i++) {
        extract_variable_data(
            data, variables, dates, i,
            cat_offsets, cat_label_idx_arr, cat_counts_arr,
            REAL(cat_freq), do_projection, &jobs[i]
        );
    }

    int analysis_error = ddiwr_run_variable_jobs(
        jobs, (size_t)n, requested_threads, DDIWR_ANALYSIS_STATS
    );


    for (i = 0; i < n; i++) {
        CVariableData *job = &jobs[i];
        const char *classification = "char";
        REAL(sum_valid)[i] = job->sum_valid;
        REAL(sum_invalid)[i] = job->sum_invalid;

        if (job->classification_label_count > 0) {
            if (job->classification_all_labels_missing) {
                if (job->source_numeric_candidate) {
                    classification = job->classification_distinct_count < 15 ?
                        "numcat" : "num";
                }
            }
            else if (job->classification_all_values_labelled) {
                classification = job->classification_labels_numeric ? "cat" : "catchar";
            }
            else {
                classification = job->classification_distinct_count < 7 ? "cat" : "catnum";
            }
        }
        else if (job->source_numeric_candidate && job->source_is_numeric) {
            classification = job->classification_distinct_count < 15 ? "numcat" : "num";
        }

        SET_STRING_ELT(variable_type, i, mkChar(classification));
        LOGICAL(weight_numeric_compatible)[i] = job->source_numeric_candidate;
        LOGICAL(weight_has_labels)[i] = job->has_labels;
        LOGICAL(weight_has_observed)[i] = job->has_observed_value;
        LOGICAL(weight_has_negative)[i] = job->has_negative_value;

        if (job->is_numericish && job->sum_valid > 0) {
            REAL(var_dcml)[i] = (double)job->max_dcml;
            REAL(var_width)[i] = (double)job->max_width;
            SET_STRING_ELT(range_units, i, mkChar(job->whole ? "INT" : "REAL"));

            if (!job->date_var && (job->sum_valid - job->sum_invalid) > 0) {
                REAL(val_min)[i] = job->val_min;
                REAL(val_max)[i] = job->val_max;
                REAL(stat_min)[i] = job->stat_min;
                REAL(stat_max)[i] = job->stat_max;
                REAL(stat_mean)[i] = job->stat_mean;
                REAL(stat_medn)[i] = job->stat_medn;
                REAL(stat_stdev)[i] = job->stat_stdev;
            }
        }
    }

    for (i = 0; i < n; i++) {
        if (jobs[i].str_data != NULL) free(jobs[i].str_data);
        if (jobs[i].cat_label_idx != NULL) free(jobs[i].cat_label_idx);
        if (jobs[i].cat_label_svals != NULL) free(jobs[i].cat_label_svals);
        if (jobs[i].cat_label_dvals != NULL) free(jobs[i].cat_label_dvals);
        if (jobs[i].cat_missing != NULL) free(jobs[i].cat_missing);
        if (jobs[i].classification_label_dvals != NULL) {
            free(jobs[i].classification_label_dvals);
        }
        if (jobs[i].classification_label_svals != NULL) {
            free(jobs[i].classification_label_svals);
        }
        if (jobs[i].classification_label_missing != NULL) {
            free(jobs[i].classification_label_missing);
        }
    }
    free(jobs);

    SET_VECTOR_ELT(out, 0, var_dcml);
    SET_VECTOR_ELT(out, 1, var_width);
    SET_VECTOR_ELT(out, 2, range_units);
    SET_VECTOR_ELT(out, 3, val_min);
    SET_VECTOR_ELT(out, 4, val_max);
    SET_VECTOR_ELT(out, 5, stat_min);
    SET_VECTOR_ELT(out, 6, stat_max);
    SET_VECTOR_ELT(out, 7, stat_mean);
    SET_VECTOR_ELT(out, 8, stat_medn);
    SET_VECTOR_ELT(out, 9, stat_stdev);
    SET_VECTOR_ELT(out, 10, sum_valid);
    SET_VECTOR_ELT(out, 11, sum_invalid);
    SET_VECTOR_ELT(out, 12, cat_counts);
    SET_VECTOR_ELT(out, 13, cat_values);
    SET_VECTOR_ELT(out, 14, cat_labels);
    SET_VECTOR_ELT(out, 15, cat_missing);
    SET_VECTOR_ELT(out, 16, cat_freq);
    SET_VECTOR_ELT(out, 17, variable_type);
    SET_VECTOR_ELT(out, 18, weight_numeric_compatible);
    SET_VECTOR_ELT(out, 19, weight_has_labels);
    SET_VECTOR_ELT(out, 20, weight_has_observed);
    SET_VECTOR_ELT(out, 21, weight_has_negative);
    SET_STRING_ELT(names, 0, mkChar("var_dcml"));
    SET_STRING_ELT(names, 1, mkChar("var_width"));
    SET_STRING_ELT(names, 2, mkChar("range_units"));
    SET_STRING_ELT(names, 3, mkChar("val_min"));
    SET_STRING_ELT(names, 4, mkChar("val_max"));
    SET_STRING_ELT(names, 5, mkChar("stat_min"));
    SET_STRING_ELT(names, 6, mkChar("stat_max"));
    SET_STRING_ELT(names, 7, mkChar("stat_mean"));
    SET_STRING_ELT(names, 8, mkChar("stat_medn"));
    SET_STRING_ELT(names, 9, mkChar("stat_stdev"));
    SET_STRING_ELT(names, 10, mkChar("sum_valid"));
    SET_STRING_ELT(names, 11, mkChar("sum_invalid"));
    SET_STRING_ELT(names, 12, mkChar("cat_counts"));
    SET_STRING_ELT(names, 13, mkChar("cat_values"));
    SET_STRING_ELT(names, 14, mkChar("cat_labels"));
    SET_STRING_ELT(names, 15, mkChar("cat_missing"));
    SET_STRING_ELT(names, 16, mkChar("cat_freq"));
    SET_STRING_ELT(names, 17, mkChar("variable_type"));
    SET_STRING_ELT(names, 18, mkChar("weight_numeric_compatible"));
    SET_STRING_ELT(names, 19, mkChar("weight_has_labels"));
    SET_STRING_ELT(names, 20, mkChar("weight_has_observed"));
    SET_STRING_ELT(names, 21, mkChar("weight_has_negative"));
    setAttrib(out, R_NamesSymbol, names);

    for (i = 0; i < n; i++) {
        free(cat_label_idx_arr[i]);
    }
    free(cat_offsets);
    free(cat_label_idx_arr);
    free(cat_counts_arr);

    UNPROTECT(24);
    if (analysis_error) {
        Rf_error("Failed to allocate variable summary working memory.");
    }
    return out;
}

static void extract_format_data(xmlmeta_result *result, CVariableData *job, R_xlen_t i) {
    SEXP x = result->x;
    SEXP classes = result->classes_attr;
    SEXP labels = result->labels;

    job->index = (int)i;
    job->type = TYPEOF(x);
    job->len = XLENGTH(x);
    job->has_labels = (labels != R_NilValue);
    job->cat_count = labels != R_NilValue ? (int)XLENGTH(labels) : 0;
    job->is_numericish = (TYPEOF(x) == REALSXP || TYPEOF(x) == INTSXP || TYPEOF(x) == LGLSXP || TYPEOF(x) == STRSXP);
    job->date_var = 0;

    job->real_data = NULL;
    job->int_data = NULL;
    job->lgl_data = NULL;
    job->str_data = NULL;

    if (TYPEOF(x) == REALSXP) {
        job->real_data = REAL(x);
    } else if (TYPEOF(x) == INTSXP) {
        job->int_data = INTEGER(x);
    } else if (TYPEOF(x) == LGLSXP) {
        job->lgl_data = LOGICAL(x);
    } else if (TYPEOF(x) == STRSXP) {
        job->str_data = (const char **)malloc((size_t)job->len * sizeof(char *));
        for (R_xlen_t j = 0; j < job->len; j++) {
            if (STRING_ELT(x, j) == NA_STRING) {
                job->str_data[j] = NULL;
            } else {
                job->str_data[j] = CHAR(STRING_ELT(x, j));
            }
        }
    }

    job->cat_label_dvals = NULL;
    job->cat_label_svals = NULL;
    if (labels != R_NilValue && job->cat_count > 0) {
        if (TYPEOF(labels) == STRSXP) {
            job->cat_label_svals = (const char **)malloc((size_t)job->cat_count * sizeof(char *));
            for (int k = 0; k < job->cat_count; k++) {
                if (STRING_ELT(labels, k) == NA_STRING) {
                    job->cat_label_svals[k] = NULL;
                } else {
                    job->cat_label_svals[k] = CHAR(STRING_ELT(labels, k));
                }
            }
        } else {
            job->cat_label_dvals = (double *)malloc((size_t)job->cat_count * sizeof(double));
            for (int k = 0; k < job->cat_count; k++) {
                double val = 0.0;
                if (sexp_as_double(labels, k, &val)) {
                    job->cat_label_dvals[k] = val;
                } else {
                    job->cat_label_dvals[k] = NA_REAL;
                }
            }
        }
    }

    if (class_has(classes, "POSIXct")) {
        snprintf(job->format_spss, sizeof(job->format_spss), "DATETIME");
        snprintf(job->format_stata, sizeof(job->format_stata), "%%tc");
        job->is_date = 0;
        job->len = 0;
    } else if (class_has(classes, "Date")) {
        job->is_date = 1;
        job->format_spss[0] = '\0';
        job->format_stata[0] = '\0';
        job->len = 0;
    } else if (class_has(classes, "hms")) {
        snprintf(job->format_spss, sizeof(job->format_spss), "TIME");
        snprintf(job->format_stata, sizeof(job->format_stata), "%%tc");
        job->is_date = 0;
        job->len = 0;
    }
}

SEXP collect_xml_metadata(SEXP data, SEXP include_formats) {
    int requested_threads = ddiwr_requested_variable_threads();
    R_xlen_t i = 0;
    R_xlen_t n = 0;
    SEXP out = R_NilValue;
    SEXP out_names = R_NilValue;
    xmlmeta_result *results = NULL;
    int do_formats = 1;

    if (!Rf_isNewList(data)) {
        Rf_error("Argument 'data' must be a list.");
    }
    if (!Rf_isLogical(include_formats) || XLENGTH(include_formats) != 1) {
        Rf_error("Argument 'include_formats' must be a logical scalar.");
    }
    do_formats = LOGICAL(include_formats)[0] != 0;

    ddiwr_init_symbols();

    n = XLENGTH(data);
    results = (xmlmeta_result *)calloc((size_t)n, sizeof(xmlmeta_result));
    if (results == NULL) {
        Rf_error("Failed to allocate metadata buffers.");
    }

    for (i = 0; i < n; i++) {
        xmlmeta_process_variable(data, results, i, 0); // Extract attributes, skip format inference sequentially
    }

    if (do_formats) {
        CVariableData *jobs = (CVariableData *)calloc((size_t)n, sizeof(CVariableData));
        if (jobs == NULL) {
            free(results);
            Rf_error("Failed to allocate format inference jobs.");
        }

        for (i = 0; i < n; i++) {
            extract_format_data(&results[i], &jobs[i], i);
        }

        ddiwr_run_variable_jobs(
            jobs, (size_t)n, requested_threads, DDIWR_ANALYSIS_FORMATS
        );


        for (i = 0; i < n; i++) {
            strcpy(results[i].format_spss, jobs[i].format_spss);
            strcpy(results[i].format_stata, jobs[i].format_stata);
            results[i].is_date = jobs[i].is_date;

            if (jobs[i].str_data != NULL) free(jobs[i].str_data);
            if (jobs[i].cat_label_svals != NULL) free(jobs[i].cat_label_svals);
            if (jobs[i].cat_label_dvals != NULL) free(jobs[i].cat_label_dvals);
        }
        free(jobs);
    }

    PROTECT(out = allocVector(VECSXP, n));
    PROTECT(out_names = getAttrib(data, R_NamesSymbol));

    for (i = 0; i < n; i++) {
        SEXP item = R_NilValue;
        SEXP item_names = R_NilValue;
        SEXP classes = results[i].classes_attr;
        int idx = 0;
        int fields = do_formats ? 5 : 4; /* classes, na_range, [varFormat], xmlang, ID */
        int has_label = results[i].label != R_NilValue;
        int has_measurement = results[i].measurement != R_NilValue;
        int has_labels = results[i].labels != R_NilValue || results[i].factor_fallback;
        int has_na_values = results[i].na_values != R_NilValue;

        if (has_label) fields++;
        if (has_measurement) fields++;
        if (has_labels) fields++;
        if (has_na_values) fields++;

        PROTECT(item = allocVector(VECSXP, fields));
        PROTECT(item_names = allocVector(STRSXP, fields));

        if (classes == R_NilValue) {
            PROTECT(classes = allocVector(STRSXP, 1));
            SET_STRING_ELT(classes, 0, mkChar(type2char(TYPEOF(results[i].x))));
        }
        else {
            PROTECT(classes);
        }
        SET_VECTOR_ELT(item, idx, classes);
        SET_STRING_ELT(item_names, idx++, mkChar("classes"));

        if (has_label) {
            SET_VECTOR_ELT(item, idx, results[i].label);
            SET_STRING_ELT(item_names, idx++, mkChar("label"));
        }

        if (has_measurement) {
            SET_VECTOR_ELT(item, idx, results[i].measurement);
            SET_STRING_ELT(item_names, idx++, mkChar("measurement"));
        }

        if (has_labels) {
            SEXP labels = results[i].labels;
            if (results[i].factor_fallback) {
                R_xlen_t k = XLENGTH(results[i].levels);
                SEXP fac_labels = PROTECT(allocVector(INTSXP, k));
                SEXP fac_names = PROTECT(allocVector(STRSXP, k));
                R_xlen_t j = 0;

                for (j = 0; j < k; j++) {
                    INTEGER(fac_labels)[j] = (int)(j + 1);
                    SET_STRING_ELT(fac_names, j, STRING_ELT(results[i].levels, j));
                }
                setAttrib(fac_labels, R_NamesSymbol, fac_names);
                labels = fac_labels;
            }
            SET_VECTOR_ELT(item, idx, labels);
            SET_STRING_ELT(item_names, idx++, mkChar("labels"));
            if (results[i].factor_fallback) {
                UNPROTECT(2);
            }
        }

        if (has_na_values) {
            SEXP na_values = PROTECT(sanitize_na_values(results[i].na_values));
            if (na_values != R_NilValue) {
                SET_VECTOR_ELT(item, idx, na_values);
                SET_STRING_ELT(item_names, idx++, mkChar("na_values"));
            }
            UNPROTECT(1);
        }

        SET_VECTOR_ELT(item, idx, results[i].na_range);
        SET_STRING_ELT(item_names, idx++, mkChar("na_range"));

        if (do_formats) {
            if (results[i].is_date) {
                SEXP fmt = PROTECT(mkString("date"));
                SET_VECTOR_ELT(item, idx, fmt);
                UNPROTECT(1);
            }
            else {
                SEXP fmt = PROTECT(allocVector(STRSXP, 2));
                SET_STRING_ELT(fmt, 0, mkChar(results[i].format_spss));
                SET_STRING_ELT(fmt, 1, mkChar(results[i].format_stata));
                SET_VECTOR_ELT(item, idx, fmt);
                UNPROTECT(1);
            }
            SET_STRING_ELT(item_names, idx++, mkChar("varFormat"));
        }

        SET_VECTOR_ELT(item, idx, results[i].xmlang);
        SET_STRING_ELT(item_names, idx++, mkChar("xmlang"));

        SET_VECTOR_ELT(item, idx, results[i].id);
        SET_STRING_ELT(item_names, idx++, mkChar("ID"));

        setAttrib(item, R_NamesSymbol, item_names);
        SET_VECTOR_ELT(out, i, item);
        UNPROTECT(3);
    }

    if (TYPEOF(out_names) == STRSXP && XLENGTH(out_names) == n) {
        setAttrib(out, R_NamesSymbol, out_names);
    }

    free(results);
    UNPROTECT(2);
    return out;
}

SEXP label_freqs(SEXP x, SEXP labels, SEXP wt) {
    R_xlen_t n = XLENGTH(x);
    R_xlen_t k = XLENGTH(labels);
    R_xlen_t i = 0;
    R_xlen_t j = 0;
    SEXP out = R_NilValue;
    int weighted = wt != R_NilValue && wt != R_NilValue && TYPEOF(wt) != NILSXP;

    if (!(TYPEOF(x) == REALSXP || TYPEOF(x) == INTSXP || TYPEOF(x) == LGLSXP || TYPEOF(x) == STRSXP)) {
        Rf_error("Argument 'x' must be an atomic vector.");
    }
    if (!(TYPEOF(labels) == REALSXP || TYPEOF(labels) == INTSXP || TYPEOF(labels) == LGLSXP || TYPEOF(labels) == STRSXP)) {
        Rf_error("Argument 'labels' must be an atomic vector.");
    }
    if (weighted && XLENGTH(wt) != n) {
        Rf_error("Argument 'wt' must have same length as 'x'.");
    }

    PROTECT(out = allocVector(REALSXP, k));
    for (j = 0; j < k; j++) {
        REAL(out)[j] = 0.0;
    }

    for (i = 0; i < n; i++) {
        int is_missing = 0;
        double w = 1.0;

        if (TYPEOF(x) == STRSXP) {
            is_missing = (STRING_ELT(x, i) == NA_STRING);
        } else if (TYPEOF(x) == REALSXP) {
            is_missing = ISNAN(REAL(x)[i]);
        } else if (TYPEOF(x) == INTSXP) {
            is_missing = INTEGER(x)[i] == NA_INTEGER;
        } else if (TYPEOF(x) == LGLSXP) {
            is_missing = LOGICAL(x)[i] == NA_LOGICAL;
        }

        if (is_missing) {
            continue;
        }

        if (weighted) {
            if (TYPEOF(wt) == REALSXP) {
                if (ISNAN(REAL(wt)[i])) {
                    continue;
                }
                w = REAL(wt)[i];
            } else if (TYPEOF(wt) == INTSXP) {
                if (INTEGER(wt)[i] == NA_INTEGER) {
                    continue;
                }
                w = (double)INTEGER(wt)[i];
            } else if (TYPEOF(wt) == LGLSXP) {
                if (LOGICAL(wt)[i] == NA_LOGICAL) {
                    continue;
                }
                w = (double)LOGICAL(wt)[i];
            } else {
                double tmp = 0.0;
                if (!sexp_as_double(wt, i, &tmp)) {
                    continue;
                }
                w = tmp;
            }
        }

        for (j = 0; j < k; j++) {
            if (value_matches_label(x, i, labels, j)) {
                REAL(out)[j] += w;
                break;
            }
        }
    }

    UNPROTECT(1);
    return out;
}

static void sb_init(ddiwr_strbuf *sb, size_t initial_cap) {
    sb->len = 0;
    sb->cap = initial_cap > 0 ? initial_cap : 1024;
    sb->buf = (char *)malloc(sb->cap);
    if (sb->buf == NULL) {
        Rf_error("Out of memory while allocating XML buffer.");
    }
    sb->buf[0] = '\0';
}

static void sb_free(ddiwr_strbuf *sb) {
    if (sb->buf != NULL) {
        free(sb->buf);
        sb->buf = NULL;
    }
    sb->len = 0;
    sb->cap = 0;
}

static void sb_reserve(ddiwr_strbuf *sb, size_t add) {
    size_t need = sb->len + add + 1;
    if (need <= sb->cap) {
        return;
    }
    while (sb->cap < need) {
        sb->cap *= 2;
    }
    sb->buf = (char *)realloc(sb->buf, sb->cap);
    if (sb->buf == NULL) {
        Rf_error("Out of memory while growing XML buffer.");
    }
}

static void sb_append(ddiwr_strbuf *sb, const char *s) {
    size_t n = strlen(s);
    sb_reserve(sb, n);
    memcpy(sb->buf + sb->len, s, n);
    sb->len += n;
    sb->buf[sb->len] = '\0';
}

static void sb_appendf(ddiwr_strbuf *sb, const char *fmt, ...) {
    va_list args;
    va_list args2;
    int needed = 0;

    va_start(args, fmt);
    va_copy(args2, args);
    needed = vsnprintf(NULL, 0, fmt, args);
    va_end(args);

    if (needed < 0) {
        va_end(args2);
        Rf_error("Failed formatting XML content.");
    }

    sb_reserve(sb, (size_t)needed);
    vsnprintf(sb->buf + sb->len, sb->cap - sb->len, fmt, args2);
    va_end(args2);
    sb->len += (size_t)needed;
}

static void sb_append_xml_escaped(ddiwr_strbuf *sb, const char *s) {
    const char *p = s;
    while (*p) {
        switch (*p) {
            case '&': sb_append(sb, "&amp;"); break;
            case '<': sb_append(sb, "&lt;"); break;
            case '>': sb_append(sb, "&gt;"); break;
            case '"': sb_append(sb, "&quot;"); break;
            case '\'': sb_append(sb, "&apos;"); break;
            default: {
                char c[2];
                c[0] = *p;
                c[1] = '\0';
                sb_append(sb, c);
            }
        }
        p++;
    }
}

static void sb_append_indent(ddiwr_strbuf *sb, int level, int indent_width) {
    int i = 0;
    int spaces = level * indent_width;
    if (spaces <= 0) {
        return;
    }
    sb_reserve(sb, (size_t)spaces);
    for (i = 0; i < spaces; i++) {
        sb->buf[sb->len++] = ' ';
    }
    sb->buf[sb->len] = '\0';
}

SEXP write_text_file(SEXP path, SEXP text) {
    FILE *fp = NULL;
    const char *cpath = NULL;
    size_t total_written = 0;
    size_t total_bytes = 0;
    R_xlen_t i = 0;

    if (!Rf_isString(path) || XLENGTH(path) != 1) {
        Rf_error("Argument 'path' must be a character scalar.");
    }

    if (!Rf_isString(text) || XLENGTH(text) < 1) {
        Rf_error("Argument 'text' must be a character vector.");
    }

    cpath = CHAR(STRING_ELT(path, 0));

    fp = fopen(cpath, "wb");
    if (fp == NULL) {
        Rf_error("Cannot open file for writing: %s", cpath);
    }

    for (i = 0; i < XLENGTH(text); i++) {
        const char *ctext = CHAR(STRING_ELT(text, i));
        size_t nbytes = strlen(ctext);
        size_t written = 0;

        total_bytes += nbytes;

        if (nbytes > 0) {
            written = fwrite(ctext, 1, nbytes, fp);
            total_written += written;
        }
    }

    if (fclose(fp) != 0) {
        Rf_error("Error while closing file: %s", cpath);
    }

    if (total_written != total_bytes) {
        Rf_error("Failed to write complete content to file: %s", cpath);
    }

    return R_NilValue;
}

SEXP make_datadscr_xml(
    SEXP ns_prefix,
    SEXP indent_width,
    SEXP base_level,
    SEXP var_names,
    SEXP var_ids,
    SEXP var_labels,
    SEXP var_dcml,
    SEXP range_units,
    SEXP val_min,
    SEXP val_max,
    SEXP inval_min,
    SEXP inval_max,
    SEXP stat_min,
    SEXP stat_max,
    SEXP stat_mean,
    SEXP stat_medn,
    SEXP stat_stdev,
    SEXP sum_valid,
    SEXP sum_invalid,
    SEXP varformat_type,
    SEXP varformat_value,
    SEXP cat_counts,
    SEXP cat_values,
    SEXP cat_labels,
    SEXP cat_missing,
    SEXP cat_freq
) {
    R_xlen_t i = 0;
    R_xlen_t n = 0;
    ddiwr_strbuf sb;
    SEXP out = R_NilValue;
    const char *nsp = NULL;
    int indent = 2;
    int level0 = 1;
    int level_var = 0;
    int level_var_child = 0;
    int level_var_grand = 0;

    if (!Rf_isString(ns_prefix) || XLENGTH(ns_prefix) != 1) {
        Rf_error("Argument 'ns_prefix' must be a character scalar.");
    }
    nsp = CHAR(STRING_ELT(ns_prefix, 0));

    if (!Rf_isInteger(indent_width) || XLENGTH(indent_width) != 1) {
        Rf_error("Argument 'indent_width' must be an integer scalar.");
    }
    if (!Rf_isInteger(base_level) || XLENGTH(base_level) != 1) {
        Rf_error("Argument 'base_level' must be an integer scalar.");
    }

    indent = INTEGER(indent_width)[0];
    level0 = INTEGER(base_level)[0];
    if (indent < 0 || level0 < 0) {
        Rf_error("Arguments 'indent_width' and 'base_level' must be non-negative.");
    }

    level_var = level0 + 1;
    level_var_child = level0 + 2;
    level_var_grand = level0 + 3;

    if (!Rf_isString(var_names)) {
        Rf_error("Argument 'var_names' must be a character vector.");
    }
    n = XLENGTH(var_names);

    if (!Rf_isString(var_ids) || XLENGTH(var_ids) != n) {
        Rf_error("Argument 'var_ids' must be a character vector with same length as 'var_names'.");
    }
    if (!Rf_isString(var_labels) || XLENGTH(var_labels) != n) {
        Rf_error("Argument 'var_labels' must be a character vector with same length as 'var_names'.");
    }
    if (!Rf_isReal(var_dcml) || XLENGTH(var_dcml) != n) {
        Rf_error("Argument 'var_dcml' must be a numeric vector with same length as 'var_names'.");
    }
    if (!Rf_isString(range_units) || XLENGTH(range_units) != n) {
        Rf_error("Argument 'range_units' must be a character vector with same length as 'var_names'.");
    }
    if (!Rf_isReal(val_min) || XLENGTH(val_min) != n) {
        Rf_error("Argument 'val_min' must be a numeric vector with same length as 'var_names'.");
    }
    if (!Rf_isReal(val_max) || XLENGTH(val_max) != n) {
        Rf_error("Argument 'val_max' must be a numeric vector with same length as 'var_names'.");
    }
    if (!Rf_isReal(inval_min) || XLENGTH(inval_min) != n) {
        Rf_error("Argument 'inval_min' must be a numeric vector with same length as 'var_names'.");
    }
    if (!Rf_isReal(inval_max) || XLENGTH(inval_max) != n) {
        Rf_error("Argument 'inval_max' must be a numeric vector with same length as 'var_names'.");
    }
    if (!Rf_isReal(stat_min) || XLENGTH(stat_min) != n) {
        Rf_error("Argument 'stat_min' must be a numeric vector with same length as 'var_names'.");
    }
    if (!Rf_isReal(stat_max) || XLENGTH(stat_max) != n) {
        Rf_error("Argument 'stat_max' must be a numeric vector with same length as 'var_names'.");
    }
    if (!Rf_isReal(stat_mean) || XLENGTH(stat_mean) != n) {
        Rf_error("Argument 'stat_mean' must be a numeric vector with same length as 'var_names'.");
    }
    if (!Rf_isReal(stat_medn) || XLENGTH(stat_medn) != n) {
        Rf_error("Argument 'stat_medn' must be a numeric vector with same length as 'var_names'.");
    }
    if (!Rf_isReal(stat_stdev) || XLENGTH(stat_stdev) != n) {
        Rf_error("Argument 'stat_stdev' must be a numeric vector with same length as 'var_names'.");
    }
    if (!Rf_isReal(sum_valid) || XLENGTH(sum_valid) != n) {
        Rf_error("Argument 'sum_valid' must be a numeric vector with same length as 'var_names'.");
    }
    if (!Rf_isReal(sum_invalid) || XLENGTH(sum_invalid) != n) {
        Rf_error("Argument 'sum_invalid' must be a numeric vector with same length as 'var_names'.");
    }
    if (!Rf_isString(varformat_type) || XLENGTH(varformat_type) != n) {
        Rf_error("Argument 'varformat_type' must be a character vector with same length as 'var_names'.");
    }
    if (!Rf_isString(varformat_value) || XLENGTH(varformat_value) != n) {
        Rf_error("Argument 'varformat_value' must be a character vector with same length as 'var_names'.");
    }
    if (!Rf_isInteger(cat_counts) || XLENGTH(cat_counts) != n) {
        Rf_error("Argument 'cat_counts' must be an integer vector with same length as 'var_names'.");
    }
    if (!Rf_isString(cat_values)) {
        Rf_error("Argument 'cat_values' must be a character vector.");
    }
    if (!Rf_isString(cat_labels)) {
        Rf_error("Argument 'cat_labels' must be a character vector.");
    }
    if (!Rf_isLogical(cat_missing)) {
        Rf_error("Argument 'cat_missing' must be a logical vector.");
    }
    if (!Rf_isReal(cat_freq)) {
        Rf_error("Argument 'cat_freq' must be a numeric vector.");
    }
    if (
        XLENGTH(cat_values) != XLENGTH(cat_labels) ||
        XLENGTH(cat_values) != XLENGTH(cat_missing) ||
        XLENGTH(cat_values) != XLENGTH(cat_freq)
    ) {
        Rf_error("Category vectors should have equal length.");
    }

    PROTECT(out = Rf_allocVector(STRSXP, n));

    R_xlen_t cat_offset = 0;

    for (i = 0; i < n; i++) {
        const char *vname = NULL;
        const char *vid = NULL;
        const char *vlab = NULL;
        const char *vunit = NULL;
        const char *vfmt_type = NULL;
        const char *vfmt_value = NULL;
        double vdcml = REAL(var_dcml)[i];
        double vmin = REAL(val_min)[i];
        double vmax = REAL(val_max)[i];
        double ivmin = REAL(inval_min)[i];
        double ivmax = REAL(inval_max)[i];
        double smin = REAL(stat_min)[i];
        double smax = REAL(stat_max)[i];
        double smean = REAL(stat_mean)[i];
        double smedn = REAL(stat_medn)[i];
        double sstdev = REAL(stat_stdev)[i];
        double sval = REAL(sum_valid)[i];
        double sinv = REAL(sum_invalid)[i];
        SEXP s_name = STRING_ELT(var_names, i);
        SEXP s_id = STRING_ELT(var_ids, i);
        SEXP s_lbl = STRING_ELT(var_labels, i);
        SEXP s_unit = STRING_ELT(range_units, i);
        SEXP s_vfmt_type = STRING_ELT(varformat_type, i);
        SEXP s_vfmt_value = STRING_ELT(varformat_value, i);
        int cat_n = INTEGER(cat_counts)[i];

        if (s_name == NA_STRING || s_id == NA_STRING) {
            UNPROTECT(1);
            Rf_error("Arguments 'var_names' and 'var_ids' should not contain NA.");
        }

        vname = CHAR(s_name);
        vid = CHAR(s_id);
        vlab = (s_lbl == NA_STRING) ? "" : CHAR(s_lbl);
        vunit = (s_unit == NA_STRING) ? "REAL" : CHAR(s_unit);
        vfmt_type = (s_vfmt_type == NA_STRING) ? "" : CHAR(s_vfmt_type);
        vfmt_value = (s_vfmt_value == NA_STRING) ? "" : CHAR(s_vfmt_value);

        sb_init(&sb, 1024);

        sb_append_indent(&sb, level_var, indent);
        sb_appendf(&sb, "<%svar", nsp);
        sb_append(&sb, " ID=\"");
        sb_append_xml_escaped(&sb, vid);
        sb_append(&sb, "\" name=\"");
        sb_append_xml_escaped(&sb, vname);
        if (R_FINITE(vdcml)) {
            sb_appendf(&sb, "\" dcml=\"%.0f", vdcml);
        }
        sb_append(&sb, "\">\n");

        if (strlen(vlab) > 0) {
            sb_append_indent(&sb, level_var_child, indent);
            sb_appendf(&sb, "<%slabl>", nsp);
            sb_append_xml_escaped(&sb, vlab);
            sb_appendf(&sb, "</%slabl>\n", nsp);
        }

        if (R_FINITE(vmin) && R_FINITE(vmax)) {
            sb_append_indent(&sb, level_var_child, indent);
            sb_appendf(&sb, "<%svalrng>\n", nsp);
            sb_append_indent(&sb, level_var_grand, indent);
            sb_appendf(&sb, "<%srange UNITS=\"%s\" min=\"%.15g\" max=\"%.15g\"/>\n", nsp, vunit, vmin, vmax);
            sb_append_indent(&sb, level_var_child, indent);
            sb_appendf(&sb, "</%svalrng>\n", nsp);
        }

        if (R_FINITE(ivmin) || R_FINITE(ivmax)) {
            sb_append_indent(&sb, level_var_child, indent);
            sb_appendf(&sb, "<%sinvalrng>\n", nsp);
            sb_append_indent(&sb, level_var_grand, indent);
            sb_appendf(&sb, "<%srange UNITS=\"%s\"", nsp, vunit);
            if (R_FINITE(ivmin)) {
                sb_appendf(&sb, " min=\"%.15g\"", ivmin);
            }
            if (R_FINITE(ivmax)) {
                sb_appendf(&sb, " max=\"%.15g\"", ivmax);
            }
            sb_append(&sb, "/>\n");
            sb_append_indent(&sb, level_var_child, indent);
            sb_appendf(&sb, "</%sinvalrng>\n", nsp);
        }

        if (R_FINITE(smin)) {
            sb_append_indent(&sb, level_var_child, indent);
            sb_appendf(
                &sb,
                "<%ssumStat type=\"min\">%.15g</%ssumStat>\n",
                nsp, smin, nsp
            );
        }

        if (R_FINITE(smax)) {
            sb_append_indent(&sb, level_var_child, indent);
            sb_appendf(
                &sb,
                "<%ssumStat type=\"max\">%.15g</%ssumStat>\n",
                nsp, smax, nsp
            );
        }

        if (R_FINITE(smean)) {
            sb_append_indent(&sb, level_var_child, indent);
            sb_appendf(
                &sb,
                "<%ssumStat type=\"mean\">%.15g</%ssumStat>\n",
                nsp, smean, nsp
            );
        }

        if (R_FINITE(smedn)) {
            sb_append_indent(&sb, level_var_child, indent);
            sb_appendf(
                &sb,
                "<%ssumStat type=\"medn\">%.15g</%ssumStat>\n",
                nsp, smedn, nsp
            );
        }

        if (R_FINITE(sstdev)) {
            sb_append_indent(&sb, level_var_child, indent);
            sb_appendf(
                &sb,
                "<%ssumStat type=\"stdev\">%.15g</%ssumStat>\n",
                nsp, sstdev, nsp
            );
        }

        if (R_FINITE(sval)) {
            sb_append_indent(&sb, level_var_child, indent);
            sb_appendf(
                &sb,
                "<%ssumStat type=\"vald\" wgtd=\"not-wgtd\">%.15g</%ssumStat>\n",
                nsp, sval, nsp
            );
        }

        if (R_FINITE(sinv)) {
            sb_append_indent(&sb, level_var_child, indent);
            sb_appendf(
                &sb,
                "<%ssumStat type=\"invd\" wgtd=\"not-wgtd\">%.15g</%ssumStat>\n",
                nsp, sinv, nsp
            );
        }

        if (cat_n < 0) {
            UNPROTECT(1);
            sb_free(&sb);
            Rf_error("Category counts should be non-negative.");
        }

        if (cat_offset + cat_n > XLENGTH(cat_values)) {
            UNPROTECT(1);
            sb_free(&sb);
            Rf_error("Category offsets exceed category vector lengths.");
        }

        for (int j = 0; j < cat_n; j++) {
            R_xlen_t idx = cat_offset + j;
            SEXP s_cat_val = STRING_ELT(cat_values, idx);
            SEXP s_cat_lab = STRING_ELT(cat_labels, idx);
            int ismiss = LOGICAL(cat_missing)[idx];
            double freq = REAL(cat_freq)[idx];
            const char *cval = (s_cat_val == NA_STRING) ? "" : CHAR(s_cat_val);
            const char *clab = (s_cat_lab == NA_STRING) ? "" : CHAR(s_cat_lab);

            sb_append_indent(&sb, level_var_child, indent);
            sb_appendf(
                &sb,
                "<%scatgry%s>\n",
                nsp,
                (ismiss == TRUE ? " missing=\"Y\"" : "")
            );

            sb_append_indent(&sb, level_var_grand, indent);
            sb_appendf(&sb, "<%scatValu>", nsp);
            sb_append_xml_escaped(&sb, cval);
            sb_appendf(&sb, "</%scatValu>\n", nsp);

            sb_append_indent(&sb, level_var_grand, indent);
            sb_appendf(&sb, "<%slabl>", nsp);
            sb_append_xml_escaped(&sb, clab);
            sb_appendf(&sb, "</%slabl>\n", nsp);

            if (R_FINITE(freq)) {
                sb_append_indent(&sb, level_var_grand, indent);
                sb_appendf(
                    &sb,
                    "<%scatStat type=\"freq\">%.15g</%scatStat>\n",
                    nsp, freq, nsp
                );
            }

            sb_append_indent(&sb, level_var_child, indent);
            sb_appendf(&sb, "</%scatgry>\n", nsp);
        }
        cat_offset += cat_n;

        if (strlen(vfmt_type) > 0 && strlen(vfmt_value) > 0) {
            sb_append_indent(&sb, level_var_child, indent);
            sb_appendf(
                &sb,
                "<%svarFormat type=\"%s\">",
                nsp, vfmt_type
            );
            sb_append_xml_escaped(&sb, vfmt_value);
            sb_appendf(&sb, "</%svarFormat>\n", nsp);
        }

        sb_append_indent(&sb, level_var, indent);
        sb_appendf(&sb, "</%svar>\n", nsp);

        SET_STRING_ELT(out, i, Rf_mkChar(sb.buf));
        sb_free(&sb);
    }

    UNPROTECT(1);
    return out;
}
