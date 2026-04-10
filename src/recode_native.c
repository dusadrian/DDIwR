#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>
#include <R_ext/Memory.h>
#include <R_ext/Utils.h>
#include <ctype.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>

static int is_string_like(SEXP x) {
    return TYPEOF(x) == STRSXP || TYPEOF(x) == CHARSXP;
}

static int same_key_ci(const char *lhs, const char *rhs) {
    unsigned char a;
    unsigned char b;

    if (lhs == NULL || rhs == NULL) {
        return 0;
    }

    while (*lhs != '\0' && *rhs != '\0') {
        a = (unsigned char) *lhs++;
        b = (unsigned char) *rhs++;
        if (tolower(a) != tolower(b)) {
            return 0;
        }
    }

    return *lhs == '\0' && *rhs == '\0';
}

static int same_key(const char *lhs, const char *rhs, int ignore_case) {
    if (ignore_case) {
        return same_key_ci(lhs, rhs);
    }
    return lhs != NULL && rhs != NULL && strcmp(lhs, rhs) == 0;
}

static int key_index(const char *key, const char **old_keys, R_xlen_t n_old, int ignore_case) {
    R_xlen_t i;

    if (key == NULL || key[0] == '\0') {
        return -1;
    }

    for (i = 0; i < n_old; ++i) {
        if (same_key(key, old_keys[i], ignore_case)) {
            return (int) i;
        }
    }

    return -1;
}

static int parse_int_value(const char *text, int *out) {
    char *endptr = NULL;
    long value;

    if (text == NULL || text[0] == '\0') {
        return 0;
    }

    value = strtol(text, &endptr, 10);
    if (endptr == text || *endptr != '\0') {
        return 0;
    }

    *out = (int) value;
    return 1;
}

static int parse_double_value(const char *text, double *out) {
    char *endptr = NULL;
    double value;

    if (text == NULL || text[0] == '\0') {
        return 0;
    }

    value = R_strtod(text, &endptr);
    if (endptr == text || *endptr != '\0') {
        return 0;
    }

    *out = value;
    return 1;
}

static const char *string_elt_or_null(SEXP x, R_xlen_t i) {
    SEXP elt = STRING_ELT(x, i);
    if (elt == NA_STRING) {
        return NULL;
    }
    return CHAR(elt);
}

typedef struct {
    double old;
    char *label;
    int count;
    int n_variables;
    int last_variable;
} MissingDictionaryEntry;

typedef struct {
    char **keys;
    R_xlen_t n_keys;
    R_xlen_t cap_keys;
} MissingGroupSet;

static int is_numeric_missing_vector(SEXP x) {
    return TYPEOF(x) == REALSXP || TYPEOF(x) == INTSXP || TYPEOF(x) == STRSXP;
}

static int parse_numeric_sexp_value(SEXP x, R_xlen_t i, double *out) {
    if (TYPEOF(x) == REALSXP) {
        if (ISNA(REAL(x)[i]) || ISNAN(REAL(x)[i])) {
            return 0;
        }
        *out = REAL(x)[i];
        return R_FINITE(*out);
    }

    if (TYPEOF(x) == INTSXP) {
        if (INTEGER(x)[i] == NA_INTEGER) {
            return 0;
        }
        *out = INTEGER(x)[i];
        return 1;
    }

    if (TYPEOF(x) == STRSXP) {
        return parse_double_value(string_elt_or_null(x, i), out);
    }

    return 0;
}

static char *dup_c_string(const char *text) {
    size_t n;
    char *out;

    if (text == NULL) {
        return NULL;
    }

    n = strlen(text);
    out = (char *) R_Calloc(n + 1, char);
    memcpy(out, text, n + 1);
    return out;
}

static void free_group_set(MissingGroupSet *set) {
    R_xlen_t i;
    if (set->keys != NULL) {
        for (i = 0; i < set->n_keys; ++i) {
            if (set->keys[i] != NULL) {
                R_Free(set->keys[i]);
            }
        }
        R_Free(set->keys);
    }
}

static int group_set_add(MissingGroupSet *set, const char *key, R_xlen_t limit) {
    R_xlen_t i;

    if (key == NULL || key[0] == '\0') {
        return 1;
    }

    for (i = 0; i < set->n_keys; ++i) {
        if (strcmp(set->keys[i], key) == 0) {
            return 1;
        }
    }

    if (set->n_keys >= limit) {
        return 0;
    }

    if (set->n_keys >= set->cap_keys) {
        set->cap_keys = set->cap_keys == 0 ? 16 : (set->cap_keys * 2);
        set->keys = (char **) R_Realloc(set->keys, (size_t) set->cap_keys, char *);
    }

    set->keys[set->n_keys] = dup_c_string(key);
    set->n_keys++;
    return 1;
}

static const char *label_for_code(SEXP labels, double code) {
    SEXP names;
    R_xlen_t i;

    if (labels == R_NilValue || XLENGTH(labels) == 0) {
        return NULL;
    }

    names = Rf_getAttrib(labels, R_NamesSymbol);
    if (TYPEOF(names) != STRSXP || XLENGTH(names) != XLENGTH(labels)) {
        return NULL;
    }

    for (i = 0; i < XLENGTH(labels); ++i) {
        double value;
        if (parse_numeric_sexp_value(labels, i, &value) && value == code) {
            return string_elt_or_null(names, i);
        }
    }

    return NULL;
}

static int add_group_key_for_code(MissingGroupSet *set, SEXP labels, double code, R_xlen_t limit) {
    const char *label = label_for_code(labels, code);
    char code_buf[64];

    if (label != NULL && label[0] != '\0') {
        return group_set_add(set, label, limit);
    }

    snprintf(code_buf, sizeof(code_buf), "%.15g", code);
    return group_set_add(set, code_buf, limit);
}

static void add_dictionary_entry(
    MissingDictionaryEntry **entries,
    R_xlen_t *n_entries,
    R_xlen_t *cap_entries,
    double old,
    const char *label,
    int variable_index
) {
    R_xlen_t i;

    for (i = 0; i < *n_entries; ++i) {
        if ((*entries)[i].old == old) {
            if (((*entries)[i].label == NULL || (*entries)[i].label[0] == '\0') &&
                label != NULL && label[0] != '\0') {
                if ((*entries)[i].label != NULL) {
                    R_Free((*entries)[i].label);
                }
                (*entries)[i].label = dup_c_string(label);
            }
            (*entries)[i].count++;
            if ((*entries)[i].last_variable != variable_index) {
                (*entries)[i].n_variables++;
                (*entries)[i].last_variable = variable_index;
            }
            return;
        }
    }

    if (*n_entries >= *cap_entries) {
        *cap_entries = *cap_entries == 0 ? 16 : (*cap_entries * 2);
        *entries = (MissingDictionaryEntry *) R_Realloc(*entries, (size_t) *cap_entries, MissingDictionaryEntry);
    }

    (*entries)[*n_entries].old = old;
    (*entries)[*n_entries].label = dup_c_string(label != NULL ? label : "");
    (*entries)[*n_entries].count = 1;
    (*entries)[*n_entries].n_variables = 1;
    (*entries)[*n_entries].last_variable = variable_index;
    (*n_entries)++;
}

static int dictionary_entry_cmp_old(const void *a, const void *b) {
    const MissingDictionaryEntry *ea = (const MissingDictionaryEntry *) a;
    const MissingDictionaryEntry *eb = (const MissingDictionaryEntry *) b;

    if (ea->old < eb->old) {
        return -1;
    }
    if (ea->old > eb->old) {
        return 1;
    }
    return 0;
}

static void reverse_dictionary_entries(MissingDictionaryEntry *entries, R_xlen_t n_entries) {
    R_xlen_t i;
    for (i = 0; i < n_entries / 2; ++i) {
        MissingDictionaryEntry tmp = entries[i];
        entries[i] = entries[n_entries - 1 - i];
        entries[n_entries - 1 - i] = tmp;
    }
}

static void collect_missing_codes_from_variable(
    SEXP x,
    MissingDictionaryEntry **entries,
    R_xlen_t *n_entries,
    R_xlen_t *cap_entries,
    int variable_index
) {
    SEXP labels = Rf_getAttrib(x, Rf_install("labels"));
    SEXP na_values = Rf_getAttrib(x, Rf_install("na_values"));
    SEXP na_range = Rf_getAttrib(x, Rf_install("na_range"));
    SEXP na_index = Rf_getAttrib(x, Rf_install("na_index"));
    SEXP na_index_names = R_NilValue;
    R_xlen_t i;

    if (is_numeric_missing_vector(na_values) && XLENGTH(na_values) > 0) {
        for (i = 0; i < XLENGTH(na_values); ++i) {
            double value;
            if (parse_numeric_sexp_value(na_values, i, &value)) {
                add_dictionary_entry(entries, n_entries, cap_entries, value, label_for_code(labels, value), variable_index);
            }
        }
    }

    if (TYPEOF(na_index) == INTSXP && XLENGTH(na_index) > 0) {
        na_index_names = Rf_getAttrib(na_index, R_NamesSymbol);
        if (TYPEOF(na_index_names) == STRSXP && XLENGTH(na_index_names) == XLENGTH(na_index)) {
            for (i = 0; i < XLENGTH(na_index_names); ++i) {
                double value;
                if (parse_numeric_sexp_value(na_index_names, i, &value)) {
                    add_dictionary_entry(entries, n_entries, cap_entries, value, label_for_code(labels, value), variable_index);
                }
            }
        }
    }

    if (is_numeric_missing_vector(na_range) && XLENGTH(na_range) == 2 && (TYPEOF(x) == INTSXP || TYPEOF(x) == REALSXP)) {
        double low;
        double high;

        if (parse_numeric_sexp_value(na_range, 0, &low) && parse_numeric_sexp_value(na_range, 1, &high)) {
            if (TYPEOF(x) == REALSXP) {
                for (i = 0; i < XLENGTH(x); ++i) {
                    double value = REAL(x)[i];
                    if (!ISNA(value) && !ISNAN(value) && value >= low && value <= high) {
                        add_dictionary_entry(entries, n_entries, cap_entries, value, label_for_code(labels, value), variable_index);
                    }
                }
            } else {
                for (i = 0; i < XLENGTH(x); ++i) {
                    int value = INTEGER(x)[i];
                    if (value != NA_INTEGER && value >= low && value <= high) {
                        add_dictionary_entry(entries, n_entries, cap_entries, value, label_for_code(labels, value), variable_index);
                    }
                }
            }

            if (is_numeric_missing_vector(labels) && XLENGTH(labels) > 0) {
                for (i = 0; i < XLENGTH(labels); ++i) {
                    double value;
                    if (parse_numeric_sexp_value(labels, i, &value) && value >= low && value <= high) {
                        add_dictionary_entry(entries, n_entries, cap_entries, value, label_for_code(labels, value), variable_index);
                    }
                }
            }
        }
    }
}

static int count_missing_groups_with_limit(SEXP dataset, R_xlen_t limit) {
    MissingGroupSet set;
    R_xlen_t col;

    memset(&set, 0, sizeof(set));

    for (col = 0; col < XLENGTH(dataset); ++col) {
        SEXP x = VECTOR_ELT(dataset, col);
        SEXP labels = Rf_getAttrib(x, Rf_install("labels"));
        SEXP na_values = Rf_getAttrib(x, Rf_install("na_values"));
        SEXP na_range = Rf_getAttrib(x, Rf_install("na_range"));
        SEXP na_index = Rf_getAttrib(x, Rf_install("na_index"));
        SEXP na_index_names = R_NilValue;
        R_xlen_t i;

        if (is_numeric_missing_vector(na_values) && XLENGTH(na_values) > 0) {
            for (i = 0; i < XLENGTH(na_values); ++i) {
                double value;
                if (parse_numeric_sexp_value(na_values, i, &value) &&
                    !add_group_key_for_code(&set, labels, value, limit)) {
                    free_group_set(&set);
                    return (int) (limit + 1);
                }
            }
        }

        if (TYPEOF(na_index) == INTSXP && XLENGTH(na_index) > 0) {
            na_index_names = Rf_getAttrib(na_index, R_NamesSymbol);
            if (TYPEOF(na_index_names) == STRSXP && XLENGTH(na_index_names) == XLENGTH(na_index)) {
                for (i = 0; i < XLENGTH(na_index_names); ++i) {
                    double value;
                    if (parse_numeric_sexp_value(na_index_names, i, &value) &&
                        !add_group_key_for_code(&set, labels, value, limit)) {
                        free_group_set(&set);
                        return (int) (limit + 1);
                    }
                }
            }
        }

        if (is_numeric_missing_vector(na_range) && XLENGTH(na_range) == 2 && (TYPEOF(x) == INTSXP || TYPEOF(x) == REALSXP)) {
            double low;
            double high;

            if (parse_numeric_sexp_value(na_range, 0, &low) && parse_numeric_sexp_value(na_range, 1, &high)) {
                if (TYPEOF(x) == REALSXP) {
                    for (i = 0; i < XLENGTH(x); ++i) {
                        double value = REAL(x)[i];
                        if (!ISNA(value) && !ISNAN(value) && value >= low && value <= high &&
                            !add_group_key_for_code(&set, labels, value, limit)) {
                            free_group_set(&set);
                            return (int) (limit + 1);
                        }
                    }
                } else {
                    for (i = 0; i < XLENGTH(x); ++i) {
                        int value = INTEGER(x)[i];
                        if (value != NA_INTEGER && value >= low && value <= high &&
                            !add_group_key_for_code(&set, labels, value, limit)) {
                            free_group_set(&set);
                            return (int) (limit + 1);
                        }
                    }
                }

                if (is_numeric_missing_vector(labels) && XLENGTH(labels) > 0) {
                    for (i = 0; i < XLENGTH(labels); ++i) {
                        double value;
                        if (parse_numeric_sexp_value(labels, i, &value) && value >= low && value <= high &&
                            !add_group_key_for_code(&set, labels, value, limit)) {
                            free_group_set(&set);
                            return (int) (limit + 1);
                        }
                    }
                }
            }
        }
    }

    free_group_set(&set);
    return (int) set.n_keys;
}

SEXP can_build_dictionary_(SEXP dataset, SEXP to, SEXP limit) {
    const char *target;
    int n_groups;
    int max_groups = Rf_asInteger(limit);

    if (TYPEOF(dataset) != VECSXP) {
        Rf_error("The input must be a data frame.");
    }

    if (TYPEOF(to) != STRSXP || XLENGTH(to) == 0) {
        Rf_error("Argument 'to' must be a character scalar.");
    }

    target = CHAR(STRING_ELT(to, 0));
    if (strcmp(target, "STATA") != 0 && strcmp(target, "SAS") != 0) {
        return Rf_ScalarLogical(1);
    }

    n_groups = count_missing_groups_with_limit(dataset, max_groups);
    return Rf_ScalarLogical(n_groups <= max_groups);
}

SEXP build_dictionary_(SEXP dataset, SEXP to, SEXP start) {
    const char *target;
    int tospss;
    int start_value;
    MissingDictionaryEntry *entries = NULL;
    R_xlen_t n_entries = 0;
    R_xlen_t cap_entries = 0;
    R_xlen_t i;
    SEXP out = R_NilValue;
    SEXP names = R_NilValue;
    SEXP row_names = R_NilValue;

    if (TYPEOF(dataset) != VECSXP) {
        Rf_error("The input must be a data frame.");
    }

    if (TYPEOF(to) != STRSXP || XLENGTH(to) == 0) {
        Rf_error("Argument 'to' must be a character scalar.");
    }

    target = CHAR(STRING_ELT(to, 0));
    tospss = strcmp(target, "SPSS") == 0;
    start_value = Rf_asInteger(start);

    for (i = 0; i < XLENGTH(dataset); ++i) {
        collect_missing_codes_from_variable(VECTOR_ELT(dataset, i), &entries, &n_entries, &cap_entries, (int) i);
    }

    qsort(entries, (size_t) n_entries, sizeof(MissingDictionaryEntry), dictionary_entry_cmp_old);
    if (n_entries > 0) {
        int all_negative = 1;
        for (i = 0; i < n_entries; ++i) {
            if (!(entries[i].old < 0)) {
                all_negative = 0;
                break;
            }
        }
        if (all_negative) {
            reverse_dictionary_entries(entries, n_entries);
        }
    }

    PROTECT(out = Rf_allocVector(VECSXP, 5));
    PROTECT(names = Rf_allocVector(STRSXP, 5));
    PROTECT(row_names = Rf_allocVector(INTSXP, 2));

    if (n_entries == 0) {
        SET_VECTOR_ELT(out, 0, Rf_allocVector(STRSXP, 0));
        SET_VECTOR_ELT(out, 1, Rf_allocVector(REALSXP, 0));
        SET_VECTOR_ELT(out, 2, tospss ? Rf_allocVector(INTSXP, 0) : Rf_allocVector(STRSXP, 0));
        SET_VECTOR_ELT(out, 3, Rf_allocVector(INTSXP, 0));
        SET_VECTOR_ELT(out, 4, Rf_allocVector(INTSXP, 0));
    } else {
        SEXP label = PROTECT(Rf_allocVector(STRSXP, n_entries));
        SEXP old = PROTECT(Rf_allocVector(REALSXP, n_entries));
        SEXP new_values = PROTECT(tospss ? Rf_allocVector(INTSXP, n_entries) : Rf_allocVector(STRSXP, n_entries));
        SEXP count = PROTECT(Rf_allocVector(INTSXP, n_entries));
        SEXP n_variables = PROTECT(Rf_allocVector(INTSXP, n_entries));

        if (!tospss && n_entries > (R_xlen_t) strlen("abcdefghijklmnopqrstuvwxyz")) {
            Rf_error("Too many overall missing values.");
        }

        for (i = 0; i < n_entries; ++i) {
            SET_STRING_ELT(label, i, Rf_mkChar(entries[i].label != NULL ? entries[i].label : ""));
            REAL(old)[i] = entries[i].old;
            INTEGER(count)[i] = entries[i].count;
            INTEGER(n_variables)[i] = entries[i].n_variables;
            if (tospss) {
                int base_code = 5000 > ((int) n_entries + 1) ? 5000 : ((int) n_entries + 1);
                int code = (int) (base_code + abs(start_value) - 1 + i);
                INTEGER(new_values)[i] = start_value < 0 ? -code : code;
            } else {
                char tag[2];
                tag[0] = (char) ('a' + i);
                tag[1] = '\0';
                SET_STRING_ELT(new_values, i, Rf_mkChar(tag));
            }
        }

        SET_VECTOR_ELT(out, 0, label);
        SET_VECTOR_ELT(out, 1, old);
        SET_VECTOR_ELT(out, 2, new_values);
        SET_VECTOR_ELT(out, 3, count);
        SET_VECTOR_ELT(out, 4, n_variables);
        UNPROTECT(5);
    }

    SET_STRING_ELT(names, 0, Rf_mkChar("label"));
    SET_STRING_ELT(names, 1, Rf_mkChar("old"));
    SET_STRING_ELT(names, 2, Rf_mkChar("new"));
    SET_STRING_ELT(names, 3, Rf_mkChar("count"));
    SET_STRING_ELT(names, 4, Rf_mkChar("n_variables"));
    Rf_setAttrib(out, R_NamesSymbol, names);

    INTEGER(row_names)[0] = NA_INTEGER;
    INTEGER(row_names)[1] = -(int) n_entries;
    Rf_setAttrib(out, R_RowNamesSymbol, row_names);
    Rf_setAttrib(out, R_ClassSymbol, Rf_mkString("data.frame"));

    if (entries != NULL) {
        for (i = 0; i < n_entries; ++i) {
            if (entries[i].label != NULL) {
                R_Free(entries[i].label);
            }
        }
        R_Free(entries);
    }

    UNPROTECT(3);
    return out;
}

SEXP all_numeric_chars_(SEXP x) {
    R_xlen_t i, n;

    if (x == R_NilValue) {
        return Rf_ScalarLogical(0);
    }

    if (TYPEOF(x) == INTSXP || TYPEOF(x) == REALSXP) {
        return Rf_ScalarLogical(1);
    }

    if (TYPEOF(x) == STRSXP) {
        n = XLENGTH(x);
        if (n == 0) {
            return Rf_ScalarLogical(0);
        }

        for (i = 0; i < n; ++i) {
            const char *text = string_elt_or_null(x, i);
            char *endptr = NULL;

            if (text == NULL || text[0] == '\0') {
                return Rf_ScalarLogical(0);
            }

            R_strtod(text, &endptr);
            if (endptr == text || *endptr != '\0') {
                return Rf_ScalarLogical(0);
            }
        }

        return Rf_ScalarLogical(1);
    }

    return Rf_ScalarLogical(0);
}

static SEXP coerce_new_strings(SEXP new_values) {
    if (TYPEOF(new_values) == STRSXP) {
        return new_values;
    }
    return Rf_coerceVector(new_values, STRSXP);
}

static int string_in_set(const char *key, SEXP values) {
    R_xlen_t i, n;
    const char *candidate;

    if (key == NULL || values == R_NilValue || TYPEOF(values) != STRSXP) {
        return 0;
    }

    n = XLENGTH(values);
    for (i = 0; i < n; ++i) {
        candidate = string_elt_or_null(values, i);
        if (candidate != NULL && strcmp(key, candidate) == 0) {
            return 1;
        }
    }

    return 0;
}

static SEXP remap_na_index(SEXP na_index, SEXP old, SEXP new_chr) {
    SEXP out = R_NilValue;
    SEXP names = R_NilValue;
    SEXP new_names = R_NilValue;
    R_xlen_t i, n, keep = 0;

    if (na_index == R_NilValue || XLENGTH(na_index) == 0) {
        return R_NilValue;
    }

    names = Rf_getAttrib(na_index, R_NamesSymbol);
    if (TYPEOF(names) != STRSXP || XLENGTH(names) != XLENGTH(na_index)) {
        return Rf_duplicate(na_index);
    }

    n = XLENGTH(na_index);
    PROTECT(out = Rf_allocVector(TYPEOF(na_index), n));
    PROTECT(new_names = Rf_allocVector(STRSXP, n));

    keep = 0;
    for (i = 0; i < n; ++i) {
        const char *name = string_elt_or_null(names, i);
        int idx = -1;
        if (name != NULL) {
            R_xlen_t j, n_old = XLENGTH(old);
            for (j = 0; j < n_old; ++j) {
                if (same_key(name, string_elt_or_null(old, j), 1)) {
                    idx = (int) j;
                    break;
                }
            }
        }
        if (idx >= 0) {
            SET_STRING_ELT(new_names, keep, STRING_ELT(new_chr, idx));
        } else if (name != NULL) {
            SET_STRING_ELT(new_names, keep, STRING_ELT(names, i));
        } else {
            SET_STRING_ELT(new_names, keep, NA_STRING);
        }

        if (TYPEOF(na_index) == INTSXP) {
            INTEGER(out)[keep] = INTEGER(na_index)[i];
        } else if (TYPEOF(na_index) == REALSXP) {
            REAL(out)[keep] = REAL(na_index)[i];
        }

        if (STRING_ELT(new_names, keep) != NA_STRING &&
            string_in_set(CHAR(STRING_ELT(new_names, keep)), new_chr)) {
            keep++;
        }
    }

    if (keep < n) {
        SEXP shrunk;
        SEXP shrunk_names;
        PROTECT(shrunk = Rf_allocVector(TYPEOF(na_index), keep));
        PROTECT(shrunk_names = Rf_allocVector(STRSXP, keep));
        for (i = 0; i < keep; ++i) {
            if (TYPEOF(na_index) == INTSXP) {
                INTEGER(shrunk)[i] = INTEGER(out)[i];
            } else if (TYPEOF(na_index) == REALSXP) {
                REAL(shrunk)[i] = REAL(out)[i];
            }
            SET_STRING_ELT(shrunk_names, i, STRING_ELT(new_names, i));
        }
        Rf_setAttrib(shrunk, R_NamesSymbol, shrunk_names);
        UNPROTECT(4);
        return shrunk;
    }

    Rf_setAttrib(out, R_NamesSymbol, new_names);
    UNPROTECT(2);
    return out;
}

static SEXP append_label_matches(SEXP na_values, SEXP labels, SEXP old, SEXP new_chr) {
    R_xlen_t i, n_old, matched = 0, na_n = 0, out_n;
    int *seen = NULL;
    SEXP out;
    SEXP na_values_chr = R_NilValue;

    if (labels == R_NilValue) {
        return na_values == R_NilValue ? R_NilValue : Rf_duplicate(na_values);
    }

    if (na_values != R_NilValue) {
        if (TYPEOF(na_values) != STRSXP) {
            PROTECT(na_values_chr = Rf_coerceVector(na_values, STRSXP));
        } else {
            PROTECT(na_values_chr = na_values);
        }
        na_n = XLENGTH(na_values_chr);
    }

    n_old = XLENGTH(old);
    seen = (int *) R_Calloc((size_t) n_old, int);

    if (TYPEOF(labels) == STRSXP || TYPEOF(labels) == REALSXP || TYPEOF(labels) == INTSXP) {
        char keybuf[128];
        for (i = 0; i < XLENGTH(labels); ++i) {
            const char *key = NULL;
            int idx = -1;

            if (TYPEOF(labels) == STRSXP) {
                key = string_elt_or_null(labels, i);
            } else if (TYPEOF(labels) == REALSXP && !ISNA(REAL(labels)[i]) && !ISNAN(REAL(labels)[i])) {
                snprintf(keybuf, sizeof(keybuf), "%.15g", REAL(labels)[i]);
                key = keybuf;
            } else if (TYPEOF(labels) == INTSXP && INTEGER(labels)[i] != NA_INTEGER) {
                snprintf(keybuf, sizeof(keybuf), "%d", INTEGER(labels)[i]);
                key = keybuf;
            }

            if (key != NULL) {
                R_xlen_t j;
                for (j = 0; j < n_old; ++j) {
                    if (same_key(key, string_elt_or_null(old, j), TYPEOF(labels) == STRSXP)) {
                        idx = (int) j;
                        break;
                    }
                }
            }

            if (idx >= 0 && !seen[idx]) {
                seen[idx] = 1;
                matched++;
            }
        }
    }

    if (matched == 0) {
        R_Free(seen);
        return na_values == R_NilValue ? R_NilValue : Rf_duplicate(na_values);
    }

    out_n = na_n + matched;
    PROTECT(out = Rf_allocVector(STRSXP, out_n));
    for (i = 0; i < na_n; ++i) {
        SET_STRING_ELT(out, i, STRING_ELT(na_values_chr, i));
    }

    matched = 0;
    for (i = 0; i < n_old; ++i) {
        if (seen[i]) {
            SET_STRING_ELT(out, na_n + matched, STRING_ELT(new_chr, i));
            matched++;
        }
    }

    R_Free(seen);
    UNPROTECT(1 + (na_values != R_NilValue ? 1 : 0));
    return out;
}

static SEXP recode_vector(SEXP x, SEXP new_chr, const char **old_keys, R_xlen_t n_old) {
    SEXP out = Rf_duplicate(x);
    R_xlen_t n = XLENGTH(out);
    R_xlen_t i;
    int index;
    const char *key;
    const char *replacement;
    int ignore_case = TYPEOF(out) == STRSXP;

    switch (TYPEOF(out)) {
        case STRSXP:
            for (i = 0; i < n; ++i) {
                key = string_elt_or_null(out, i);
                index = key_index(key, old_keys, n_old, ignore_case);
                if (index >= 0) {
                    SET_STRING_ELT(out, i, STRING_ELT(new_chr, index));
                }
            }
            break;

        case INTSXP: {
            int *old_int_ok = (int *) R_alloc((size_t) n_old, sizeof(int));
            int *old_int = (int *) R_alloc((size_t) n_old, sizeof(int));
            int *new_int_ok = (int *) R_alloc((size_t) n_old, sizeof(int));
            int *new_int = (int *) R_alloc((size_t) n_old, sizeof(int));

            for (i = 0; i < n_old; ++i) {
                old_int_ok[i] = parse_int_value(old_keys[i], &old_int[i]);
                new_int_ok[i] = parse_int_value(string_elt_or_null(new_chr, i), &new_int[i]);
            }

            for (i = 0; i < n; ++i) {
                R_xlen_t j;
                if (INTEGER(out)[i] == NA_INTEGER) {
                    continue;
                }
                index = -1;
                for (j = 0; j < n_old; ++j) {
                    if (old_int_ok[j] && INTEGER(out)[i] == old_int[j]) {
                        index = (int) j;
                        break;
                    }
                }
                if (index >= 0) {
                    if (new_int_ok[index]) {
                        INTEGER(out)[i] = new_int[index];
                    } else {
                        INTEGER(out)[i] = NA_INTEGER;
                    }
                }
            }
            break;
        }

        case REALSXP: {
            int *old_double_ok = (int *) R_alloc((size_t) n_old, sizeof(int));
            double *old_double = (double *) R_alloc((size_t) n_old, sizeof(double));
            int *new_double_ok = (int *) R_alloc((size_t) n_old, sizeof(int));
            double *new_double = (double *) R_alloc((size_t) n_old, sizeof(double));

            for (i = 0; i < n_old; ++i) {
                old_double_ok[i] = parse_double_value(old_keys[i], &old_double[i]);
                new_double_ok[i] = parse_double_value(string_elt_or_null(new_chr, i), &new_double[i]);
            }

            for (i = 0; i < n; ++i) {
                R_xlen_t j;
                if (ISNA(REAL(out)[i]) || ISNAN(REAL(out)[i])) {
                    continue;
                }
                index = -1;
                for (j = 0; j < n_old; ++j) {
                    if (old_double_ok[j] && REAL(out)[i] == old_double[j]) {
                        index = (int) j;
                        break;
                    }
                }
                if (index >= 0) {
                    if (new_double_ok[index]) {
                        REAL(out)[i] = new_double[index];
                    } else {
                        REAL(out)[i] = NA_REAL;
                    }
                }
            }
            break;
        }

        default:
            break;
    }

    return out;
}

SEXP recode_to_spss_(SEXP x, SEXP labels, SEXP na_values, SEXP old, SEXP new_values) {
    SEXP out;
    SEXP names;
    SEXP x_new;
    SEXP labels_new = R_NilValue;
    SEXP na_values_new = R_NilValue;
    SEXP new_chr;
    const char **old_keys;
    R_xlen_t n_old;
    R_xlen_t i;

    if (TYPEOF(old) != STRSXP) {
        old = PROTECT(Rf_coerceVector(old, STRSXP));
    } else {
        PROTECT(old);
    }

    new_chr = PROTECT(coerce_new_strings(new_values));
    n_old = XLENGTH(old);
    old_keys = (const char **) R_alloc((size_t) n_old, sizeof(const char *));

    for (i = 0; i < n_old; ++i) {
        old_keys[i] = string_elt_or_null(old, i);
    }

    x_new = PROTECT(recode_vector(x, new_chr, old_keys, n_old));

    if (labels != R_NilValue) {
        labels_new = PROTECT(recode_vector(labels, new_chr, old_keys, n_old));
    }

    if (na_values != R_NilValue) {
        na_values_new = PROTECT(recode_vector(na_values, new_chr, old_keys, n_old));
    }

    out = PROTECT(Rf_allocVector(VECSXP, 3));
    names = PROTECT(Rf_allocVector(STRSXP, 3));

    SET_VECTOR_ELT(out, 0, x_new);
    SET_VECTOR_ELT(out, 1, labels_new);
    SET_VECTOR_ELT(out, 2, na_values_new);

    SET_STRING_ELT(names, 0, Rf_mkChar("x"));
    SET_STRING_ELT(names, 1, Rf_mkChar("labels"));
    SET_STRING_ELT(names, 2, Rf_mkChar("na_values"));
    Rf_setAttrib(out, R_NamesSymbol, names);

    UNPROTECT(
        5 +
        (labels != R_NilValue ? 1 : 0) +
        (na_values != R_NilValue ? 1 : 0)
    );
    return out;
}

SEXP recode_to_spss_full_(SEXP x, SEXP labels, SEXP na_values, SEXP na_index, SEXP old, SEXP new_values) {
    SEXP out;
    SEXP names;
    SEXP x_new;
    SEXP labels_new = R_NilValue;
    SEXP na_values_new = R_NilValue;
    SEXP na_index_new = R_NilValue;
    SEXP new_chr;
    const char **old_keys;
    R_xlen_t n_old;
    R_xlen_t i;

    if (TYPEOF(old) != STRSXP) {
        old = PROTECT(Rf_coerceVector(old, STRSXP));
    } else {
        PROTECT(old);
    }

    new_chr = PROTECT(coerce_new_strings(new_values));
    n_old = XLENGTH(old);
    old_keys = (const char **) R_alloc((size_t) n_old, sizeof(const char *));

    for (i = 0; i < n_old; ++i) {
        old_keys[i] = string_elt_or_null(old, i);
    }

    x_new = PROTECT(recode_vector(x, new_chr, old_keys, n_old));

    if (labels != R_NilValue) {
        labels_new = PROTECT(recode_vector(labels, new_chr, old_keys, n_old));
    }

    if (na_values != R_NilValue) {
        SEXP recoded = PROTECT(recode_vector(na_values, new_chr, old_keys, n_old));
        na_values_new = PROTECT(append_label_matches(recoded, labels, old, new_chr));
        UNPROTECT(1);
    } else if (labels != R_NilValue) {
        na_values_new = PROTECT(append_label_matches(R_NilValue, labels, old, new_chr));
    }

    if (na_index != R_NilValue) {
        na_index_new = PROTECT(remap_na_index(na_index, old, new_chr));
    }

    out = PROTECT(Rf_allocVector(VECSXP, 4));
    names = PROTECT(Rf_allocVector(STRSXP, 4));

    SET_VECTOR_ELT(out, 0, x_new);
    SET_VECTOR_ELT(out, 1, labels_new);
    SET_VECTOR_ELT(out, 2, na_values_new);
    SET_VECTOR_ELT(out, 3, na_index_new);

    SET_STRING_ELT(names, 0, Rf_mkChar("x"));
    SET_STRING_ELT(names, 1, Rf_mkChar("labels"));
    SET_STRING_ELT(names, 2, Rf_mkChar("na_values"));
    SET_STRING_ELT(names, 3, Rf_mkChar("na_index"));
    Rf_setAttrib(out, R_NamesSymbol, names);

    UNPROTECT(
        5 +
        (labels != R_NilValue ? 1 : 0) +
        ((na_values != R_NilValue || labels != R_NilValue) ? 1 : 0) +
        (na_index != R_NilValue ? 1 : 0)
    );
    return out;
}
