#ifndef DDIWR_STATA_CHARACTERISTICS_H
#define DDIWR_STATA_CHARACTERISTICS_H

#include <limits.h>
#include <stdlib.h>
#include <string.h>

static int ddiwr_stata_note_name(const char *name) {
    char *end;
    long index;
    if (strncmp(name, "note", 4) || name[4] < '1' || name[4] > '9') {
        return 0;
    }
    index = strtol(name + 4, &end, 10);
    return !*end && index <= INT_MAX;
}

static void ddiwr_attach_stata_characteristics(SEXP output, SEXP column_names,
        const readstat_attribute_t *characteristics) {
    const readstat_attribute_t *characteristic;
    R_xlen_t note_count = 0;
    for (characteristic = characteristics; characteristic; characteristic = characteristic->next) {
        SEXP target = output;
        SEXP previous, values, names;
        R_xlen_t i, n;
        if (characteristic->variable) {
            target = R_NilValue;
            for (i = 0; i < XLENGTH(column_names); i++) {
                if (!strcmp(characteristic->variable, CHAR(STRING_ELT(column_names, i)))) {
                    target = VECTOR_ELT(output, i);
                    break;
                }
            }
            if (target == R_NilValue) {
                continue;
            }
        }
        previous = Rf_getAttrib(target, Rf_install("stata.characteristics"));
        n = previous == R_NilValue ? 0 : XLENGTH(previous);
        PROTECT(values = Rf_allocVector(VECSXP, n + 1));
        PROTECT(names = Rf_allocVector(STRSXP, n + 1));
        for (i = 0; i < n; i++) {
            SET_VECTOR_ELT(values, i, VECTOR_ELT(previous, i));
            SET_STRING_ELT(names, i, STRING_ELT(Rf_getAttrib(previous, R_NamesSymbol), i));
        }
        SET_VECTOR_ELT(values, n, Rf_ScalarString(Rf_mkCharCE(characteristic->values[0], CE_UTF8)));
        SET_STRING_ELT(names, n, Rf_mkCharCE(characteristic->name, CE_UTF8));
        Rf_setAttrib(values, R_NamesSymbol, names);
        Rf_setAttrib(target, Rf_install("stata.characteristics"), values);
        UNPROTECT(2);

        if (!characteristic->variable && ddiwr_stata_note_name(characteristic->name) &&
                strspn(characteristic->values[0], " ") != strlen(characteristic->values[0])) {
            note_count++;
        }
    }

    /* Keep the existing dataset notes view identical in serial and parallel
     * imports. Raw note characteristics above retain their exact contents. */
    if (note_count > 0) {
        SEXP notes = PROTECT(Rf_allocVector(STRSXP, note_count));
        R_xlen_t i = 0;
        for (characteristic = characteristics; characteristic; characteristic = characteristic->next) {
            if (!characteristic->variable && ddiwr_stata_note_name(characteristic->name)) {
                const char *value = characteristic->values[0];
                size_t length = strlen(value);
                while (length && value[length - 1] == ' ') {
                    length--;
                }
                if (length) {
                    SET_STRING_ELT(notes, i++, Rf_mkCharLenCE(value, length, CE_UTF8));
                }
            }
        }
        Rf_setAttrib(output, Rf_install("notes"), notes);
        UNPROTECT(1);
    }
}

#endif
