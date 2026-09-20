#ifndef DDIWR_VARIABLE_ANALYSIS_H
#define DDIWR_VARIABLE_ANALYSIS_H

#include <stddef.h>

/* Storage codes match R's atomic types; the core has no dependency on R. */
enum {
    DDIWR_LOGICAL = 10,
    DDIWR_INTEGER = 13,
    DDIWR_DOUBLE = 14,
    DDIWR_STRING = 16
};

typedef struct {
    int error;
    int index;
    int type;
    ptrdiff_t len;
    int is_numericish;
    int has_type_num;
    int date_var;
    int has_labels;
    int cat_count;

    // Classification inputs and shared scan results
    int classification_label_count;
    double *classification_label_dvals;
    const char **classification_label_svals;
    int *classification_label_missing;
    int classification_labels_numeric;
    int source_numeric_candidate;
    int source_is_numeric;
    int source_has_observed;
    int include_projection;
    int classification_distinct_count;
    int classification_all_values_labelled;
    int classification_all_labels_missing;

    // Reusable weight eligibility facts
    int has_observed_value;
    int has_negative_value;

    // Contiguous primitive pointers
    const double *real_data;
    const int *int_data;
    const int *lgl_data;
    const char **str_data;

    // Missing values
    int num_na_values_n;
    double num_na_values[3];
    int has_num_na_range;
    double num_na_range[2];
    int str_na_values_n;
    const char *str_na_values[3];

    // Category labels
    double *cat_label_dvals;
    const char **cat_label_svals;
    ptrdiff_t *cat_label_idx;
    int *cat_missing;

    // Output variables for Stats
    double sum_valid;
    double sum_invalid;
    int max_dcml;
    int max_width;
    int whole;
    double val_min;
    double val_max;
    double stat_min;
    double stat_max;
    double stat_mean;
    double stat_medn;
    double stat_stdev;
    double *cat_freq; // Pointer to output segment

    // Output variables for Formats
    char format_spss[64];
    char format_stata[64];
    int is_date;
} CVariableData;

/* Return nonzero on allocation failure; callers handle errors after joining. */
int ddiwr_process_stats(CVariableData *job);
void ddiwr_infer_formats(CVariableData *job);
int ddiwr_decimal_count(double value);
int ddiwr_is_whole_double(double value);
double ddiwr_na_real(void);

#endif
