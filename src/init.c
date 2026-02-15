#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

extern SEXP ddiwr_write_text_file(SEXP path, SEXP text);
extern SEXP ddiwr_make_datadscr_xml(
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
);

static const R_CallMethodDef CallEntries[] = {
    {"ddiwr_write_text_file", (DL_FUNC) &ddiwr_write_text_file, 2},
    {"ddiwr_make_datadscr_xml", (DL_FUNC) &ddiwr_make_datadscr_xml, 26},
    {NULL, NULL, 0}
};

void R_init_DDIwR(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
