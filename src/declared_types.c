#include <string.h>
#include <math.h>

#include "declared_types.h"

FileVendor extVendor(FileExt ext) {
    switch (ext) {
        case DECLARED_DTA:
            return DECLARED_STATA;
        case DECLARED_SAV:
        case DECLARED_POR:
            return DECLARED_SPSS;
        case DECLARED_SAS7BDAT:
        case DECLARED_SAS7BCAT:
        case DECLARED_XPT:
            return DECLARED_SAS;
        default:
            Rf_error("Unknown file extension.");
    }
    return DECLARED_SAS;
}

const char *formatAttribute(FileVendor vendor) {
    switch (vendor) {
        case DECLARED_STATA:
            return "format.stata";
        case DECLARED_SPSS:
            return "format.spss";
        case DECLARED_SAS:
            return "format.sas";
    }
    return "";
}

int hasPrefix(const char *x, const char *prefix) {
    size_t prefix_len = strlen(prefix);
    return strncmp(x, prefix, prefix_len) == 0;
}

VarType numTypeFromSEXP(SEXP x) {
    if (Rf_inherits(x, "Date")) {
        return DECLARED_DATE;
    }
    if (Rf_inherits(x, "POSIXct")) {
        return DECLARED_DATETIME;
    }
    if (Rf_inherits(x, "hms")) {
        return DECLARED_TIME;
    }
    return DECLARED_DEFAULT;
}

VarType numTypeFromFormat(FileVendor vendor, const char *var_format) {
    if (var_format == NULL) {
        return DECLARED_DEFAULT;
    }

    switch (vendor) {
        case DECLARED_SAS:
            if      (hasPrefix(var_format, "DATETIME")) return DECLARED_DATETIME;
            else if (hasPrefix(var_format, "DT"))       return DECLARED_DATETIME;
            else if (hasPrefix(var_format, "DATEAMPM")) return DECLARED_DATETIME;
            else if (hasPrefix(var_format, "MDYAMPM"))  return DECLARED_DATETIME;
            else if (hasPrefix(var_format, "NLDATMT"))  return DECLARED_DATETIME;
            else if (hasPrefix(var_format, "NLDATM"))   return DECLARED_DATETIME;
            else if (hasPrefix(var_format, "IS8601DN")) return DECLARED_DATETIME;
            else if (hasPrefix(var_format, "IS8601DT")) return DECLARED_DATETIME;
            else if (hasPrefix(var_format, "IS8601DZ")) return DECLARED_DATETIME;
            else if (hasPrefix(var_format, "B8601DN"))  return DECLARED_DATETIME;
            else if (hasPrefix(var_format, "B8601DT"))  return DECLARED_DATETIME;
            else if (hasPrefix(var_format, "B8601DX"))  return DECLARED_DATETIME;
            else if (hasPrefix(var_format, "B8601DZ"))  return DECLARED_DATETIME;
            else if (hasPrefix(var_format, "B8601LX"))  return DECLARED_DATETIME;
            else if (hasPrefix(var_format, "E8601DN"))  return DECLARED_DATETIME;
            else if (hasPrefix(var_format, "E8601DT"))  return DECLARED_DATETIME;
            else if (hasPrefix(var_format, "E8601DX"))  return DECLARED_DATETIME;
            else if (hasPrefix(var_format, "E8601DZ"))  return DECLARED_DATETIME;
            else if (hasPrefix(var_format, "E8601LX"))  return DECLARED_DATETIME;
            else if (hasPrefix(var_format, "DATE"))     return DECLARED_DATE;
            else if (hasPrefix(var_format, "NLDATE"))   return DECLARED_DATE;
            else if (hasPrefix(var_format, "DOWNAME"))  return DECLARED_DATE;
            else if (hasPrefix(var_format, "IS8601DA")) return DECLARED_DATE;
            else if (hasPrefix(var_format, "B8601DA"))  return DECLARED_DATE;
            else if (hasPrefix(var_format, "E8601DA"))  return DECLARED_DATE;
            else if (hasPrefix(var_format, "DAY"))      return DECLARED_DATE;
            else if (hasPrefix(var_format, "WEEK"))     return DECLARED_DATE;
            else if (hasPrefix(var_format, "MON"))      return DECLARED_DATE;
            else if (hasPrefix(var_format, "QTR"))      return DECLARED_DATE;
            else if (hasPrefix(var_format, "YEAR"))     return DECLARED_DATE;
            else if (hasPrefix(var_format, "MMDDYY"))   return DECLARED_DATE;
            else if (hasPrefix(var_format, "DDMMYY"))   return DECLARED_DATE;
            else if (hasPrefix(var_format, "YYMMDD"))   return DECLARED_DATE;
            else if (hasPrefix(var_format, "MMYY"))     return DECLARED_DATE;
            else if (hasPrefix(var_format, "YYMM"))     return DECLARED_DATE;
            else if (hasPrefix(var_format, "YY"))       return DECLARED_DATE;
            else if (hasPrefix(var_format, "WORDDAT"))  return DECLARED_DATE;
            else if (hasPrefix(var_format, "NENGO"))    return DECLARED_DATE;
            else if (hasPrefix(var_format, "JULIAN"))   return DECLARED_DATE;
            else if (hasPrefix(var_format, "JULDAY"))   return DECLARED_DATE;
            else if (hasPrefix(var_format, "PDJULG"))   return DECLARED_DATE;
            else if (hasPrefix(var_format, "PDJULI"))   return DECLARED_DATE;
            else if (hasPrefix(var_format, "TIME"))     return DECLARED_TIME;
            else if (hasPrefix(var_format, "TIMEAMPM")) return DECLARED_TIME;
            else if (hasPrefix(var_format, "NLTIM"))    return DECLARED_TIME;
            else if (hasPrefix(var_format, "TOD"))      return DECLARED_TIME;
            else if (hasPrefix(var_format, "HOUR"))     return DECLARED_TIME;
            else if (hasPrefix(var_format, "HHMM"))     return DECLARED_TIME;
            else if (hasPrefix(var_format, "MMSS"))     return DECLARED_TIME;
            else if (hasPrefix(var_format, "IS8601T"))  return DECLARED_TIME;
            else if (hasPrefix(var_format, "B8601T"))   return DECLARED_TIME;
            else if (hasPrefix(var_format, "B8601LZ"))  return DECLARED_TIME;
            else if (hasPrefix(var_format, "E8601T"))   return DECLARED_TIME;
            else if (hasPrefix(var_format, "E8601LZ"))  return DECLARED_TIME;
            else                                        return DECLARED_DEFAULT;
        case DECLARED_SPSS:
            if      (hasPrefix(var_format, "DATETIME")) return DECLARED_DATETIME;
            else if (hasPrefix(var_format, "DATE"))     return DECLARED_DATE;
            else if (hasPrefix(var_format, "ADATE"))    return DECLARED_DATE;
            else if (hasPrefix(var_format, "EDATE"))    return DECLARED_DATE;
            else if (hasPrefix(var_format, "JDATE"))    return DECLARED_DATE;
            else if (hasPrefix(var_format, "SDATE"))    return DECLARED_DATE;
            else if (hasPrefix(var_format, "TIME"))     return DECLARED_TIME;
            else if (hasPrefix(var_format, "DTIME"))    return DECLARED_TIME;
            else                                        return DECLARED_DEFAULT;
        case DECLARED_STATA:
            if      (hasPrefix(var_format, "%tC"))      return DECLARED_DATETIME;
            else if (hasPrefix(var_format, "%tc"))      return DECLARED_DATETIME;
            else if (hasPrefix(var_format, "%td"))      return DECLARED_DATE;
            else if (hasPrefix(var_format, "%d"))       return DECLARED_DATE;
            else                                        return DECLARED_DEFAULT;
    }
    return DECLARED_DEFAULT;
}

static int daysOffset(FileVendor vendor) {
    switch (vendor) {
        case DECLARED_SAS:
        case DECLARED_STATA:
            return 3653;
        case DECLARED_SPSS:
            return 141428;
    }
    return 0;
}

double adjustDatetimeToR(FileVendor vendor, VarType var, double value) {
    double offset;

    if (ISNAN(value)) {
        return value;
    }

    offset = (double) daysOffset(vendor);
    switch (var) {
        case DECLARED_DATETIME:
            if (vendor == DECLARED_STATA) {
                value /= 1000.0;
            }
            return value - offset * 86400.0;
        case DECLARED_DATE:
            if (vendor == DECLARED_SPSS) {
                value /= 86400.0;
            }
            return value - offset;
        default:
            return value;
    }
}

double adjustDatetimeFromR(FileVendor vendor, SEXP col, double value) {
    double offset;

    if (ISNAN(value)) {
        return value;
    }

    offset = (double) daysOffset(vendor);
    switch (numTypeFromSEXP(col)) {
        case DECLARED_DATETIME:
            value += offset * 86400.0;
            if (vendor == DECLARED_STATA) {
                value *= 1000.0;
            }
            return value;
        case DECLARED_DATE:
            value += offset;
            if (vendor == DECLARED_SPSS) {
                value *= 86400.0;
            }
            return value;
        default:
            return value;
    }
}
