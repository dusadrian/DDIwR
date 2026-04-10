#include <R.h>
#include <Rinternals.h>
#include <R_ext/Utils.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdarg.h>
#include <math.h>

typedef struct {
    char *buf;
    size_t len;
    size_t cap;
} ddiwr_strbuf;

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

static int is_whole_double(double x) {
    if (!R_finite(x)) {
        return 0;
    }
    return fabs(x - nearbyint(x)) < 1e-12;
}

static int digit_count_floor_abs(double x) {
    double ax = fabs(x);
    double floored = floor(ax);
    int digits = 1;
    while (floored >= 10.0) {
        floored /= 10.0;
        digits++;
    }
    return digits;
}

static int decimal_count(double x) {
    char buf[128];
    char *dot = NULL;
    char *end = NULL;

    if (!R_finite(x) || is_whole_double(x)) {
        return 0;
    }

    snprintf(buf, sizeof(buf), "%.15f", x);
    dot = strchr(buf, '.');
    if (dot == NULL) {
        return 0;
    }
    end = buf + strlen(buf) - 1;
    while (end > dot && *end == '0') {
        *end = '\0';
        end--;
    }
    return (int)(end - dot);
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

SEXP collect_datadscr_stats(SEXP data, SEXP variables, SEXP dates) {
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
    R_xlen_t cat_offset = 0;

    if (!Rf_isNewList(data) || !Rf_isNewList(variables)) {
        Rf_error("Arguments 'data' and 'variables' must be lists.");
    }

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

    PROTECT(out = allocVector(VECSXP, 17));
    PROTECT(names = allocVector(STRSXP, 17));
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
        SEXP x = VECTOR_ELT(data, i);
        SEXP metadata = VECTOR_ELT(variables, i);
        SEXP labels = getListElement(metadata, "labels");
        SEXP label_names = (labels == R_NilValue) ? R_NilValue : getAttrib(labels, R_NamesSymbol);
        SEXP na_values = getListElement(metadata, "na_values");
        SEXP na_range = getListElement(metadata, "na_range");
        SEXP type = getListElement(metadata, "type");
        R_xlen_t len = XLENGTH(x);
        R_xlen_t valid_n = 0;
        R_xlen_t valid_obs = 0;
        R_xlen_t invalid_n = 0;
        R_xlen_t j = 0;
        int numericish = 1;
        int whole = 1;
        int max_dcml = 0;
        int max_width = 1;
        double *vals = NULL;
        double minv = 0.0, maxv = 0.0;
        double mean = 0.0, m2 = 0.0;
        int printnum = 0;
        int has_type_num = 0;
        int date_var = LOGICAL(dates)[i] == TRUE;
        int has_labels = labels != R_NilValue && TYPEOF(label_names) == STRSXP;
        int cat_count = 0;

        if (!(TYPEOF(x) == REALSXP || TYPEOF(x) == INTSXP || TYPEOF(x) == LGLSXP || TYPEOF(x) == STRSXP)) {
            numericish = 0;
        }

        if (type != R_NilValue && TYPEOF(type) == STRSXP && XLENGTH(type) > 0) {
            const char *ct = CHAR(STRING_ELT(type, 0));
            if (strstr(ct, "num") != NULL) {
                has_type_num = 1;
            }
        }

        if (numericish) {
            vals = (double *)R_alloc((size_t)len, sizeof(double));
        }

        if (has_labels) {
            for (j = 0; j < XLENGTH(labels); j++) {
                if (STRING_ELT(label_names, j) != NA_STRING && strlen(CHAR(STRING_ELT(label_names, j))) > 0) {
                    cat_count++;
                }
            }
            INTEGER(cat_counts)[i] = cat_count;
        }

        for (j = 0; j < len; j++) {
            int is_invalid = 0;
            double val = 0.0;

            if (TYPEOF(x) == STRSXP) {
                is_invalid = (STRING_ELT(x, j) == NA_STRING);
            } else if (TYPEOF(x) == REALSXP) {
                is_invalid = ISNAN(REAL(x)[j]);
            } else if (TYPEOF(x) == INTSXP) {
                is_invalid = INTEGER(x)[j] == NA_INTEGER;
            } else if (TYPEOF(x) == LGLSXP) {
                is_invalid = LOGICAL(x)[j] == NA_LOGICAL;
            } else {
                is_invalid = 1;
            }

            if (!is_invalid && (value_in_na_values(x, j, na_values) || value_in_na_range(x, j, na_range))) {
                is_invalid = 1;
            }

            if (is_invalid) {
                invalid_n++;
                continue;
            }

            valid_obs++;

            if (!numericish || !sexp_as_double(x, j, &val)) {
                numericish = 0;
                continue;
            }

            vals[valid_n] = val;
            if (valid_n == 0) {
                minv = maxv = val;
            } else {
                if (val < minv) minv = val;
                if (val > maxv) maxv = val;
            }

            if (!is_whole_double(val)) {
                whole = 0;
            }
            if (decimal_count(val) > max_dcml) {
                max_dcml = decimal_count(val);
            }
            if (digit_count_floor_abs(val) > max_width) {
                max_width = digit_count_floor_abs(val);
            }

            valid_n++;
        }

        REAL(sum_valid)[i] = (double)valid_obs;
        REAL(sum_invalid)[i] = (double)invalid_n;

        if (numericish && valid_n > 0) {
            size_t k = 0;
            double *sorted = (double *)R_alloc((size_t)valid_n, sizeof(double));
            memcpy(sorted, vals, (size_t)valid_n * sizeof(double));
            R_rsort(sorted, (int)valid_n);

            REAL(var_dcml)[i] = (double)max_dcml;
            REAL(var_width)[i] = (double)max_width;
            SET_STRING_ELT(range_units, i, mkChar(whole ? "INT" : "REAL"));

            if (!date_var && valid_n > 1) {
                R_xlen_t distinct_nonlabel = 0;
                double last = NA_REAL;
                int have_last = 0;

                REAL(val_min)[i] = minv;
                REAL(val_max)[i] = maxv;

                for (k = 0; k < (size_t)valid_n; k++) {
                    double cur = sorted[k];
                    int is_new = (!have_last || cur != last);
                    if (is_new) {
                        int is_label = 0;
                        R_xlen_t lj = 0;
                        if (labels != R_NilValue) {
                            for (lj = 0; lj < XLENGTH(labels); lj++) {
                                double lnum = 0.0;
                                if (sexp_as_double(labels, lj, &lnum) && cur == lnum) {
                                    is_label = 1;
                                    break;
                                }
                            }
                        }
                        if (!is_label) {
                            distinct_nonlabel++;
                        }
                        last = cur;
                        have_last = 1;
                    }
                }

                printnum = distinct_nonlabel > 4 || (valid_n > 2 && has_type_num);
                if (printnum) {
                    double nobs = 0.0;
                    double median = NA_REAL;

                    for (k = 0; k < (size_t)valid_n; k++) {
                        double xk = vals[k];
                        nobs += 1.0;
                        {
                            double delta = xk - mean;
                            mean += delta / nobs;
                            m2 += delta * (xk - mean);
                        }
                    }

                    if ((valid_n % 2) == 1) {
                        median = sorted[(size_t)(valid_n / 2)];
                    } else {
                        median = (sorted[(size_t)(valid_n / 2) - 1] + sorted[(size_t)(valid_n / 2)]) / 2.0;
                    }

                    REAL(stat_min)[i] = minv;
                    REAL(stat_max)[i] = maxv;
                    REAL(stat_mean)[i] = mean;
                    REAL(stat_medn)[i] = median;
                    if (valid_n > 1) {
                        REAL(stat_stdev)[i] = sqrt(m2 / ((double)valid_n - 1.0));
                    }
                }
            }
        }

        if (has_labels) {
            R_xlen_t pos = cat_offset;
            for (j = 0; j < XLENGTH(labels); j++) {
                const char *lnm = NULL;
                double freq = 0.0;
                R_xlen_t r = 0;

                if (STRING_ELT(label_names, j) == NA_STRING) {
                    continue;
                }
                lnm = CHAR(STRING_ELT(label_names, j));
                if (*lnm == '\0') {
                    continue;
                }

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

                for (r = 0; r < len; r++) {
                    if (value_matches_label(x, r, labels, j)) {
                        freq += 1.0;
                    }
                }
                REAL(cat_freq)[pos] = freq;
                pos++;
            }
            cat_offset += (R_xlen_t)cat_count;
        }
    }

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
    setAttrib(out, R_NamesSymbol, names);

    UNPROTECT(19);
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

static const char *find_placeholder(const char *xml, const char **matched, size_t *mlen) {
    const char *p = strstr(xml, "<dataDscr/>");
    if (p != NULL) {
        *matched = "<dataDscr/>";
        *mlen = strlen(*matched);
        return p;
    }

    p = strstr(xml, "<dataDscr />");
    if (p != NULL) {
        *matched = "<dataDscr />";
        *mlen = strlen(*matched);
        return p;
    }

    p = strstr(xml, "<d1:dataDscr/>");
    if (p != NULL) {
        *matched = "<d1:dataDscr/>";
        *mlen = strlen(*matched);
        return p;
    }

    p = strstr(xml, "<d1:dataDscr />");
    if (p != NULL) {
        *matched = "<d1:dataDscr />";
        *mlen = strlen(*matched);
        return p;
    }

    *matched = NULL;
    *mlen = 0;
    return NULL;
}

SEXP write_text_file(SEXP path, SEXP text) {
    FILE *fp = NULL;
    const char *cpath = NULL;
    const char *ctext = NULL;
    size_t written = 0;
    size_t nbytes = 0;

    if (!Rf_isString(path) || XLENGTH(path) != 1) {
        Rf_error("Argument 'path' must be a character scalar.");
    }

    if (!Rf_isString(text) || XLENGTH(text) != 1) {
        Rf_error("Argument 'text' must be a character scalar.");
    }

    cpath = CHAR(STRING_ELT(path, 0));
    ctext = CHAR(STRING_ELT(text, 0));
    nbytes = strlen(ctext);

    fp = fopen(cpath, "wb");
    if (fp == NULL) {
        Rf_error("Cannot open file for writing: %s", cpath);
    }

    if (nbytes > 0) {
        written = fwrite(ctext, 1, nbytes, fp);
    }

    if (fclose(fp) != 0) {
        Rf_error("Error while closing file: %s", cpath);
    }

    if (written != nbytes) {
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

        if (R_FINITE(ivmin) && R_FINITE(ivmax)) {
            sb_append_indent(&sb, level_var_child, indent);
            sb_appendf(&sb, "<%sinvalrng>\n", nsp);
            sb_append_indent(&sb, level_var_grand, indent);
            sb_appendf(&sb, "<%srange UNITS=\"%s\" min=\"%.15g\" max=\"%.15g\"/>\n", nsp, vunit, ivmin, ivmax);
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
