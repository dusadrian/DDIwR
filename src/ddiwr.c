#include <R.h>
#include <Rinternals.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdarg.h>

typedef struct {
    char *buf;
    size_t len;
    size_t cap;
} ddiwr_strbuf;

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

SEXP ddiwr_write_text_file(SEXP path, SEXP text) {
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

SEXP ddiwr_make_datadscr_xml(
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
