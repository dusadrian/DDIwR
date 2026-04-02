#ifndef DECLARED_TYPES_H
#define DECLARED_TYPES_H

#include <R.h>
#include <Rinternals.h>

typedef enum {
    DECLARED_SPSS = 0,
    DECLARED_STATA = 1,
    DECLARED_SAS = 2
} FileVendor;

typedef enum {
    DECLARED_SAV = 0,
    DECLARED_POR = 1,
    DECLARED_DTA = 2,
    DECLARED_SAS7BDAT = 3,
    DECLARED_SAS7BCAT = 4,
    DECLARED_XPT = 5
} FileExt;

typedef enum {
    DECLARED_DEFAULT = 0,
    DECLARED_DATE = 1,
    DECLARED_TIME = 2,
    DECLARED_DATETIME = 3
} VarType;

FileVendor extVendor(FileExt ext);
const char *formatAttribute(FileVendor vendor);
int hasPrefix(const char *x, const char *prefix);
VarType numTypeFromSEXP(SEXP x);
VarType numTypeFromFormat(FileVendor vendor, const char *var_format);
double adjustDatetimeToR(FileVendor vendor, VarType var, double value);
double adjustDatetimeFromR(FileVendor vendor, SEXP col, double value);

#endif
