#ifndef DDIWR_VENDOR_METADATA_H
#define DDIWR_VENDOR_METADATA_H

#include <R.h>
#include <Rinternals.h>
#include "readstat.h"
#include "declared_types.h"

static inline const char *ddiwr_metadata_attribute(FileVendor vendor) {
    switch (vendor) {
        case DECLARED_SPSS: return "spss_metadata";
        case DECLARED_STATA: return "stata_metadata";
        default: return "sas_metadata";
    }
}

static inline SEXP ddiwr_metadata_effective_field(SEXP object, FileVendor vendor, const char *field, const char *legacy) {
    SEXP metadata = Rf_getAttrib(object, Rf_install(ddiwr_metadata_attribute(vendor)));
    SEXP names = Rf_getAttrib(metadata, R_NamesSymbol);
    if (TYPEOF(metadata) != VECSXP || TYPEOF(names) != STRSXP) {
        return legacy ? Rf_getAttrib(object, Rf_install(legacy)) : R_NilValue;
    }
    if (XLENGTH(names) != XLENGTH(metadata)) {
        return legacy ? Rf_getAttrib(object, Rf_install(legacy)) : R_NilValue;
    }
    for (R_xlen_t i = 0; i < XLENGTH(metadata); i++) {
        if (STRING_ELT(names, i) != NA_STRING && !strcmp(CHAR(STRING_ELT(names, i)), field)) {
            return VECTOR_ELT(metadata, i);
        }
    }
    return legacy ? Rf_getAttrib(object, Rf_install(legacy)) : R_NilValue;
}

static inline SEXP ddiwr_metadata_field(SEXP object, FileVendor vendor, const char *field) {
    return ddiwr_metadata_effective_field(object, vendor, field, NULL);
}

static inline SEXP ddiwr_metadata_list(int count, const char *const *field_names) {
    SEXP result = PROTECT(Rf_allocVector(VECSXP, count));
    SEXP names = PROTECT(Rf_allocVector(STRSXP, count));
    for (int i = 0; i < count; i++) {
        SET_STRING_ELT(names, i, Rf_mkChar(field_names[i]));
    }
    Rf_setAttrib(result, R_NamesSymbol, names);
    UNPROTECT(2);
    return result;
}

/* Copy before appending: parallel reader schemas can share the original list. */
static inline void ddiwr_metadata_set_field(SEXP object, FileVendor vendor, const char *field, SEXP value) {
    SEXP old = Rf_getAttrib(object, Rf_install(ddiwr_metadata_attribute(vendor)));
    SEXP old_names = Rf_getAttrib(old, R_NamesSymbol);
    R_xlen_t count = TYPEOF(old) == VECSXP ? XLENGTH(old) : 0;
    R_xlen_t index = count;
    for (R_xlen_t i = 0; i < count; i++) {
        if (!strcmp(CHAR(STRING_ELT(old_names, i)), field)) index = i;
    }
    R_xlen_t size = count + (index == count);
    SEXP result = PROTECT(Rf_allocVector(VECSXP, size));
    SEXP names = PROTECT(Rf_allocVector(STRSXP, size));
    for (R_xlen_t i = 0; i < count; i++) {
        SET_VECTOR_ELT(result, i, VECTOR_ELT(old, i));
        SET_STRING_ELT(names, i, STRING_ELT(old_names, i));
    }
    SET_VECTOR_ELT(result, index, value);
    SET_STRING_ELT(names, index, Rf_mkChar(field));
    Rf_setAttrib(result, R_NamesSymbol, names);
    Rf_setAttrib(object, Rf_install(ddiwr_metadata_attribute(vendor)), result);
    UNPROTECT(2);
}

static inline SEXP ddiwr_metadata_time(time_t value) {
    SEXP result = PROTECT(Rf_ScalarReal((double)value));
    SEXP classes = PROTECT(Rf_allocVector(STRSXP, 2));
    SET_STRING_ELT(classes, 0, Rf_mkChar("POSIXct"));
    SET_STRING_ELT(classes, 1, Rf_mkChar("POSIXt"));
    Rf_setAttrib(result, R_ClassSymbol, classes);
    Rf_setAttrib(result, Rf_install("tzone"), Rf_mkString("UTC"));
    UNPROTECT(2);
    return result;
}

static inline SEXP ddiwr_mr_sets(readstat_metadata_t *metadata) {
    size_t count = readstat_get_multiple_response_sets_length(metadata);
    const mr_set_t *sets = readstat_get_multiple_response_sets(metadata);
    const char *fields[] = { "type", "label", "is_dichotomy", "counted_value",
        "counted_string", "variables", "label_from_var_label" };
    SEXP result = PROTECT(Rf_allocVector(VECSXP, count));
    SEXP names = PROTECT(Rf_allocVector(STRSXP, count));

    for (size_t i = 0; i < count; i++) {
        const mr_set_t *set = &sets[i];
        char type[] = { set->type, '\0' };
        SEXP entry = PROTECT(ddiwr_metadata_list(7, fields));
        SEXP variables = PROTECT(Rf_allocVector(STRSXP, set->num_subvars));
        SET_STRING_ELT(names, i, Rf_mkCharCE(set->name, CE_UTF8));
        SET_VECTOR_ELT(entry, 0, Rf_mkString(type));
        SET_VECTOR_ELT(entry, 1, Rf_ScalarString(Rf_mkCharCE(set->label ? set->label : "", CE_UTF8)));
        SET_VECTOR_ELT(entry, 2, Rf_ScalarLogical(set->is_dichotomy));
        if (set->is_dichotomy) {
            SET_VECTOR_ELT(entry, 3, Rf_ScalarInteger(set->counted_value));
            if (set->counted_string) {
                SET_VECTOR_ELT(entry, 4, Rf_ScalarString(Rf_mkCharCE(set->counted_string, CE_UTF8)));
            }
        }
        for (int j = 0; j < set->num_subvars; j++) {
            SET_STRING_ELT(variables, j, Rf_mkCharCE(set->subvariables[j], CE_UTF8));
        }
        SET_VECTOR_ELT(entry, 5, variables);
        SET_VECTOR_ELT(entry, 6, Rf_ScalarLogical(set->label_from_var_label));
        SET_VECTOR_ELT(result, i, entry);
        UNPROTECT(2);
    }
    Rf_setAttrib(result, R_NamesSymbol, names);
    UNPROTECT(2);
    return result;
}

/* Statistical file metadata has one canonical vendor-specific home. */
static inline SEXP ddiwr_dataset_metadata(readstat_metadata_t *metadata, FileExt ext) {
    const char *fields[5];
    int has_created = readstat_has_creation_time(metadata);
    int has_modified = readstat_has_modified_time(metadata);
    const char *table_name = readstat_get_table_name(metadata);
    int has_name = extVendor(ext) == DECLARED_SAS && table_name && table_name[0];
    int has_sets = extVendor(ext) == DECLARED_SPSS &&
        readstat_get_multiple_response_sets_length(metadata) > 0;
    int version = readstat_get_file_format_version(metadata);
    int count = 0;
    if (ext == DECLARED_DTA) {
        switch (version) {
            case 113: version = 8; break;
            case 114: version = 10; break;
            case 115: version = 12; break;
            case 117: version = 13; break;
            case 118: version = 14; break;
            case 119: version = 15; break;
            default: version = 0; break;
        }
    } else if (ext != DECLARED_XPT) {
        version = 0;
    }
    if (has_created) fields[count++] = "created";
    if (has_modified) fields[count++] = "modified";
    if (version) fields[count++] = "version";
    if (has_name) fields[count++] = "table_name";
    if (has_sets) fields[count++] = "mr_sets";

    SEXP result = PROTECT(ddiwr_metadata_list(count, fields));
    int index = 0;
    if (has_created) SET_VECTOR_ELT(result, index++, ddiwr_metadata_time(readstat_get_creation_time(metadata)));
    if (has_modified) SET_VECTOR_ELT(result, index++, ddiwr_metadata_time(readstat_get_modified_time(metadata)));
    if (version) SET_VECTOR_ELT(result, index++, Rf_ScalarInteger(version));
    if (has_name) SET_VECTOR_ELT(result, index++, Rf_ScalarString(Rf_mkCharCE(table_name, CE_UTF8)));
    if (has_sets) SET_VECTOR_ELT(result, index++, ddiwr_mr_sets(metadata));
    UNPROTECT(1);
    return result;
}

static inline SEXP ddiwr_column_metadata(readstat_variable_t *variable, FileVendor vendor) {
    const char *fields[8];
    const char *format = readstat_variable_get_format(variable);
    int string_type = readstat_variable_get_type_class(variable) == READSTAT_TYPE_CLASS_STRING;
    int display_width = readstat_variable_get_display_width(variable);
    const char *measure = NULL, *alignment = NULL, *informat = NULL, *type = NULL;
    int count = 0, width = 0;
    if (string_type) {
        width = readstat_variable_get_storage_width(variable);
        /* Stata's ReadStat string buffer includes a terminating byte. */
        if (vendor == DECLARED_STATA && width > 0) width--;
    }
    if (vendor == DECLARED_SPSS) {
        switch (readstat_variable_get_measure(variable)) {
            case READSTAT_MEASURE_NOMINAL: measure = "nominal"; break;
            case READSTAT_MEASURE_ORDINAL: measure = "ordinal"; break;
            case READSTAT_MEASURE_SCALE: measure = "scale"; break;
            default: break;
        }
        switch (readstat_variable_get_alignment(variable)) {
            case READSTAT_ALIGNMENT_LEFT: alignment = "left"; break;
            case READSTAT_ALIGNMENT_CENTER: alignment = "center"; break;
            case READSTAT_ALIGNMENT_RIGHT: alignment = "right"; break;
            default: break;
        }
    } else if (vendor == DECLARED_SAS) {
        informat = readstat_variable_get_informat(variable);
        if (informat && !informat[0]) informat = NULL;
        if (readstat_variable_get_type_class(variable) == READSTAT_TYPE_CLASS_NUMERIC) {
            width = readstat_variable_get_storage_width(variable);
        }
    } else {
        switch (readstat_variable_get_type(variable)) {
            case READSTAT_TYPE_INT8: type = "byte"; break;
            case READSTAT_TYPE_INT16: type = "int"; break;
            case READSTAT_TYPE_INT32: type = "long"; break;
            case READSTAT_TYPE_FLOAT: type = "float"; break;
            case READSTAT_TYPE_DOUBLE: type = "double"; break;
            case READSTAT_TYPE_STRING_REF: type = "strL"; break;
            case READSTAT_TYPE_STRING:
                /* ReadStat exposes strL as a string with zero storage width. */
                if (readstat_variable_get_storage_width(variable) == 0) type = "strL";
                break;
            default: break;
        }
    }
    if (format && format[0]) fields[count++] = "format";
    if (vendor == DECLARED_SPSS && display_width != 8) fields[count++] = "display_width";
    if (measure) fields[count++] = "measure";
    if (alignment) fields[count++] = "alignment";
    if (informat) fields[count++] = "informat";
    if (width) fields[count++] = "width";
    if (type) fields[count++] = "type";
    if (!count) return R_NilValue;

    SEXP result = PROTECT(ddiwr_metadata_list(count, fields));
    int index = 0;
    if (format && format[0]) SET_VECTOR_ELT(result, index++, Rf_mkString(format));
    if (vendor == DECLARED_SPSS && display_width != 8) SET_VECTOR_ELT(result, index++, Rf_ScalarInteger(display_width));
    if (measure) SET_VECTOR_ELT(result, index++, Rf_mkString(measure));
    if (alignment) SET_VECTOR_ELT(result, index++, Rf_mkString(alignment));
    if (informat) SET_VECTOR_ELT(result, index++, Rf_mkString(informat));
    if (width) SET_VECTOR_ELT(result, index++, Rf_ScalarInteger(width));
    if (type) SET_VECTOR_ELT(result, index++, Rf_mkString(type));
    UNPROTECT(1);
    return result;
}

#endif
