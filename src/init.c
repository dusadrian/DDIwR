#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

extern SEXP write_text_file(SEXP path, SEXP text);
extern SEXP all_numeric_chars_(SEXP x);
extern SEXP can_build_dictionary_(SEXP dataset, SEXP to, SEXP limit);
extern SEXP build_dictionary_(SEXP dataset, SEXP to, SEXP start);
extern SEXP recode_to_spss_(SEXP x, SEXP labels, SEXP na_values, SEXP old, SEXP new_values);
extern SEXP recode_to_spss_full_(SEXP x, SEXP labels, SEXP na_values, SEXP na_index, SEXP old, SEXP new_values);
extern SEXP make_datadscr_xml(
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
extern SEXP collect_xml_metadata(SEXP data, SEXP include_formats);
extern SEXP collect_datadscr_stats(SEXP data, SEXP variables, SEXP dates);
extern SEXP label_freqs(SEXP x, SEXP labels, SEXP wt);
extern SEXP declared_df_parse_dta_file(SEXP spec, SEXP encoding, SEXP cols_skip, SEXP n_max, SEXP rows_skip);
extern SEXP declared_df_parse_dta_file_parallel(SEXP spec, SEXP encoding, SEXP cols_skip, SEXP n_max, SEXP rows_skip, SEXP num_threads);
extern SEXP declared_sav_parallel_prototype(SEXP spec, SEXP num_threads);
extern SEXP declared_df_parse_sav_file_parallel_prototype(SEXP spec, SEXP encoding, SEXP user_na, SEXP num_threads);
extern SEXP declared_df_parse_por_file(SEXP spec, SEXP encoding, SEXP user_na, SEXP cols_skip, SEXP n_max, SEXP rows_skip);
extern SEXP declared_df_parse_sas_file(SEXP spec_b7dat, SEXP spec_b7cat, SEXP encoding, SEXP catalog_encoding, SEXP cols_skip, SEXP n_max, SEXP rows_skip);
extern SEXP declared_df_parse_sav_file(SEXP spec, SEXP encoding, SEXP user_na, SEXP cols_skip, SEXP n_max, SEXP rows_skip);
extern SEXP declared_df_parse_xpt_file(SEXP spec, SEXP cols_skip, SEXP n_max, SEXP rows_skip);
extern SEXP declared_write_dta_(SEXP data, SEXP path, SEXP version, SEXP label, SEXP strl_threshold, SEXP dictionary);
extern SEXP declared_write_sas_(SEXP data, SEXP path, SEXP dictionary);
extern SEXP declared_write_sav_(SEXP data, SEXP path, SEXP compress);
extern SEXP declared_write_xpt_(SEXP data, SEXP path, SEXP version, SEXP name, SEXP label, SEXP dictionary);

static const R_CallMethodDef CallEntries[] = {
    {"write_text_file", (DL_FUNC) &write_text_file, 2},
    {"all_numeric_chars_", (DL_FUNC) &all_numeric_chars_, 1},
    {"can_build_dictionary_", (DL_FUNC) &can_build_dictionary_, 3},
    {"build_dictionary_", (DL_FUNC) &build_dictionary_, 3},
    {"recode_to_spss_", (DL_FUNC) &recode_to_spss_, 5},
    {"recode_to_spss_full_", (DL_FUNC) &recode_to_spss_full_, 6},
    {"make_datadscr_xml", (DL_FUNC) &make_datadscr_xml, 26},
    {"collect_xml_metadata", (DL_FUNC) &collect_xml_metadata, 2},
    {"collect_datadscr_stats", (DL_FUNC) &collect_datadscr_stats, 3},
    {"label_freqs", (DL_FUNC) &label_freqs, 3},
    {"declared_df_parse_dta_file", (DL_FUNC) &declared_df_parse_dta_file, 5},
    {"declared_df_parse_dta_file_parallel", (DL_FUNC) &declared_df_parse_dta_file_parallel, 6},
    {"declared_sav_parallel_prototype", (DL_FUNC) &declared_sav_parallel_prototype, 2},
    {"declared_df_parse_sav_file_parallel_prototype", (DL_FUNC) &declared_df_parse_sav_file_parallel_prototype, 4},
    {"declared_df_parse_por_file", (DL_FUNC) &declared_df_parse_por_file, 6},
    {"declared_df_parse_sas_file", (DL_FUNC) &declared_df_parse_sas_file, 7},
    {"declared_df_parse_sav_file", (DL_FUNC) &declared_df_parse_sav_file, 6},
    {"declared_df_parse_xpt_file", (DL_FUNC) &declared_df_parse_xpt_file, 4},
    {"declared_write_dta_", (DL_FUNC) &declared_write_dta_, 6},
    {"declared_write_sas_", (DL_FUNC) &declared_write_sas_, 3},
    {"declared_write_sav_", (DL_FUNC) &declared_write_sav_, 3},
    {"declared_write_xpt_", (DL_FUNC) &declared_write_xpt_, 6},
    {NULL, NULL, 0}
};

void R_init_DDIwR(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
