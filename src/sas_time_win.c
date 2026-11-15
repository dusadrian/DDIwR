#include <R.h>
#include <Rinternals.h>
#include <time.h>

/* Use R's timezone database on Windows, where the C runtime cannot interpret
 * the IANA TZ values that R accepts. These calls run on the R thread. */
int ddiwr_sas_localtime(time_t timestamp, struct tm *local, int *offset) {
    int error = 0;
    SEXP stamp = PROTECT(Rf_ScalarReal((double) timestamp));
    SEXP zone = PROTECT(Rf_mkString(""));
    SEXP origin = PROTECT(Rf_mkString("1970-01-01"));
    SEXP call = PROTECT(Rf_lang4(Rf_install("as.POSIXlt"), stamp, zone, origin));
    SEXP result = PROTECT(R_tryEvalSilent(call, R_BaseEnv, &error));

    if (error || TYPEOF(result) != VECSXP || XLENGTH(result) < 11 ||
            !R_FINITE(Rf_asReal(VECTOR_ELT(result, 0)))) {
        UNPROTECT(5);
        return 0;
    }

    local->tm_sec = (int) Rf_asReal(VECTOR_ELT(result, 0));
    local->tm_min = Rf_asInteger(VECTOR_ELT(result, 1));
    local->tm_hour = Rf_asInteger(VECTOR_ELT(result, 2));
    local->tm_mday = Rf_asInteger(VECTOR_ELT(result, 3));
    local->tm_mon = Rf_asInteger(VECTOR_ELT(result, 4));
    local->tm_year = Rf_asInteger(VECTOR_ELT(result, 5));
    *offset = Rf_asInteger(VECTOR_ELT(result, 10));
    if (*offset == NA_INTEGER) {
        UNPROTECT(5);
        return 0;
    }

    UNPROTECT(5);
    return 1;
}

int ddiwr_sas_mktime(const struct tm *local, time_t *timestamp) {
    int error = 0;
    const int fields[] = {local->tm_year + 1900, local->tm_mon + 1,
        local->tm_mday, local->tm_hour, local->tm_min, local->tm_sec};
    SEXP arguments = PROTECT(Rf_allocList(6));
    SEXP argument = arguments;
    for (int i = 0; i < 6; i++, argument = CDR(argument)) {
        SETCAR(argument, Rf_ScalarInteger(fields[i]));
    }
    SEXP call = PROTECT(Rf_lcons(Rf_install("ISOdatetime"), arguments));
    SEXP result = PROTECT(R_tryEvalSilent(call, R_BaseEnv, &error));
    double seconds = error ? NA_REAL : Rf_asReal(result);

    UNPROTECT(3);
    if (!R_FINITE(seconds)) {
        return 0;
    }
    *timestamp = (time_t) seconds;
    return 1;
}
