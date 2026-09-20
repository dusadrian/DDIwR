#include "variable_analysis.h"

#include <stdlib.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include <limits.h>

/* R's NA payload is part of the binary contract, distinct from an ordinary NaN. */
double ddiwr_na_real(void) {
    union { uint64_t bits; double value; } na = {
        .bits = UINT64_C(0x7ff00000000007a2)
    };
    return na.value;
}

int ddiwr_is_whole_double(double x) {
    if (!isfinite(x)) {
        return 0;
    }
    return fabs(x - nearbyint(x)) < 1e-12;
}

int ddiwr_decimal_count(double x) {
    char buf[128];
    char *dot = NULL;
    char *end = NULL;

    if (!isfinite(x) || ddiwr_is_whole_double(x)) {
        return 0;
    }

    snprintf(buf, sizeof(buf), "%.15f", x);
    dot = strchr(buf, '.');
    if (dot == NULL) {
        return 0;
    }
    end = buf + strlen(buf) - 1;
    while (end > dot && *end == '0') {
        *end = '\0';
        end--;
    }
    return (int)(end - dot);
}

static int compare_doubles(const void *a, const void *b) {
    double da = *(const double *)a;
    double db = *(const double *)b;
    if (isnan(da) && isnan(db)) return 0;
    if (isnan(da)) return 1;
    if (isnan(db)) return -1;
    if (da < db) return -1;
    if (da > db) return 1;
    return 0;
}

static int c_value_in_na_values(double val, const char *str_val, const CVariableData *job) {
    if (job->type == DDIWR_STRING) {
        if (str_val == NULL) return 0;
        for (int k = 0; k < job->str_na_values_n; k++) {
            if (job->str_na_values[k] != NULL && strcmp(str_val, job->str_na_values[k]) == 0) {
                return 1;
            }
        }
    } else {
        if (isnan(val)) return 0;
        for (int k = 0; k < job->num_na_values_n; k++) {
            if (!isnan(job->num_na_values[k]) && val == job->num_na_values[k]) {
                return 1;
            }
        }
    }
    return 0;
}

static int c_value_in_na_range(double val, const CVariableData *job) {
    if (job->type == DDIWR_STRING || !job->has_num_na_range || isnan(val)) {
        return 0;
    }
    double lo = job->num_na_range[0];
    double hi = job->num_na_range[1];
    if (isinf(lo) && lo < 0) {
        return val <= hi;
    }
    if (isinf(hi) && hi > 0) {
        return val >= lo;
    }
    return val >= lo && val <= hi;
}

static int c_value_matches_label(double val, const char *str_val, const CVariableData *job, int cat_idx) {
    if (job->type == DDIWR_STRING) {
        if (str_val == NULL || job->cat_label_svals[cat_idx] == NULL) {
            return 0;
        }
        return strcmp(str_val, job->cat_label_svals[cat_idx]) == 0;
    } else {
        if (isnan(val) || isnan(job->cat_label_dvals[cat_idx])) {
            return 0;
        }
        return val == job->cat_label_dvals[cat_idx];
    }
}

static int c_double_matches_label_value(double val, const CVariableData *job) {
    if (job->cat_label_dvals == NULL) {
        return 0;
    }
    for (int k = 0; k < job->cat_count; k++) {
        if (!isnan(job->cat_label_dvals[k]) && val == job->cat_label_dvals[k]) {
            return 1;
        }
    }
    return 0;
}

static int c_value_matches_classification_label(
    double val,
    const char *str_val,
    const CVariableData *job,
    int label_idx
) {
    double comparable = val;

    if (job->classification_label_svals != NULL && str_val != NULL) {
        const char *label = job->classification_label_svals[label_idx];

        if (label != NULL && strcmp(str_val, label) == 0) {
            return 1;
        }
    }

    if (job->classification_label_dvals != NULL) {
        double label = job->classification_label_dvals[label_idx];

        if (str_val != NULL) {
            char *endptr = NULL;

            comparable = strtod(str_val, &endptr);
            if (endptr == str_val || *endptr != '\0') {
                comparable = NAN;
            }
        }

        if (!isnan(comparable) && !isnan(label) && comparable == label) {
            return 1;
        }
    }

    return 0;
}

static int c_display_width(double val, int type, const char *str_val) {
    char buf[128];
    if (type == DDIWR_DOUBLE) {
        if (isnan(val)) return 0;
        snprintf(buf, sizeof(buf), "%.15g", val);
        return (int)strlen(buf);
    } else if (type == DDIWR_INTEGER) {
        if (isnan(val)) return 0;
        snprintf(buf, sizeof(buf), "%d", (int)val);
        return (int)strlen(buf);
    } else if (type == DDIWR_LOGICAL) {
        if (isnan(val)) return 0;
        return ((int)val) ? 4 : 5;
    } else if (type == DDIWR_STRING) {
        if (str_val == NULL) return 0;
        return (int)strlen(str_val);
    }
    return 0;
}

void ddiwr_infer_formats(CVariableData *job) {
    int pN = 0;
    int allnax = 1;
    int nullabels = !job->has_labels;
    int decimals = 0;
    int numeric_width = 1;
    int maxvarchar = 0;
    ptrdiff_t i = 0;

    job->is_date = 0;
    pN = (job->type != DDIWR_STRING);
    if (!nullabels) {
        int labels_numeric = 1;
        if (job->cat_label_svals != NULL) {
            for (int k = 0; k < job->cat_count; k++) {
                if (job->cat_label_svals[k] != NULL) {
                    char *endptr = NULL;
                    (void)strtod(job->cat_label_svals[k], &endptr);
                    if (endptr == job->cat_label_svals[k] || *endptr != '\0') {
                        labels_numeric = 0;
                        break;
                    }
                }
            }
        }
        pN = pN && labels_numeric;
    }

    for (i = 0; i < job->len; i++) {
        if (job->type == DDIWR_STRING) {
            if (job->str_data[i] != NULL) {
                allnax = 0;
                break;
            }
        } else if (job->type == DDIWR_DOUBLE) {
            if (!isnan(job->real_data[i])) {
                allnax = 0;
                break;
            }
        } else if (job->type == DDIWR_INTEGER) {
            if (job->int_data[i] != INT_MIN) {
                allnax = 0;
                break;
            }
        } else if (job->type == DDIWR_LOGICAL) {
            if (job->lgl_data[i] != INT_MIN) {
                allnax = 0;
                break;
            }
        }
    }

    if (pN && !allnax) {
        for (i = 0; i < job->len; i++) {
            double val = 0.0;
            int width = 0;

            if (job->type == DDIWR_DOUBLE) {
                val = job->real_data[i];
                if (isnan(val)) continue;
            } else if (job->type == DDIWR_INTEGER) {
                int iv = job->int_data[i];
                if (iv == INT_MIN) continue;
                val = (double)iv;
            } else if (job->type == DDIWR_LOGICAL) {
                int lv = job->lgl_data[i];
                if (lv == INT_MIN) continue;
                val = (double)lv;
            }

            width = c_display_width(val, job->type, NULL);
            if (width > numeric_width) {
                numeric_width = width;
            }

            if (decimals < 3) {
                int d = ddiwr_decimal_count(val);
                if (d > decimals) {
                    decimals = d > 3 ? 3 : d;
                }
            }
        }
    }

    if (!pN && !allnax) {
        for (i = 0; i < job->len; i++) {
            int width = 0;
            if (job->type == DDIWR_STRING) {
                if (job->str_data[i] != NULL) {
                    width = (int)strlen(job->str_data[i]);
                }
            }
            if (width > maxvarchar) {
                maxvarchar = width;
            }
        }
    }

    if (!nullabels && !pN) {
        for (int k = 0; k < job->cat_count; k++) {
            int width = 0;
            if (job->cat_label_svals != NULL && job->cat_label_svals[k] != NULL) {
                width = (int)strlen(job->cat_label_svals[k]);
            } else if (job->cat_label_dvals != NULL) {
                char buf[128];
                snprintf(buf, sizeof(buf), "%.15g", job->cat_label_dvals[k]);
                width = (int)strlen(buf);
            }
            if (width > maxvarchar) {
                maxvarchar = width;
            }
        }
    }

    if (pN) {
        snprintf(job->format_spss, sizeof(job->format_spss), "F%d.%d", numeric_width, decimals);
        snprintf(job->format_stata, sizeof(job->format_stata), "%%%d.%dg", numeric_width, decimals);
    }
    else {
        int width = maxvarchar > 0 ? maxvarchar : 1;
        snprintf(job->format_spss, sizeof(job->format_spss), "A%d", width);
        snprintf(job->format_stata, sizeof(job->format_stata), "%%%ds", width);
    }
}

int ddiwr_process_stats(CVariableData *job) {
    ptrdiff_t len = job->len;
    ptrdiff_t valid_n = 0;
    ptrdiff_t valid_obs = 0;
    ptrdiff_t invalid_n = 0;
    int numericish = job->is_numericish;
    int whole = 1;
    int max_dcml = 0;
    int max_width = 1;
    double *vals = NULL;
    double minv = 0.0, maxv = 0.0;
    double mean = 0.0, m2 = 0.0;
    int printnum = 0;
    int distinct_nonlabel_n = 0;
    double distinct_nonlabel[5];
    double distinct_numeric[15];
    const char *distinct_string[15];

    if (job->include_projection) {
        job->classification_distinct_count = 0;
        job->classification_all_values_labelled = job->classification_label_count > 0;
        job->classification_all_labels_missing = job->classification_label_count > 0;
        job->source_has_observed = 0;
        job->has_observed_value = 0;
        job->has_negative_value = 0;

        for (int label_idx = 0; label_idx < job->classification_label_count; label_idx++) {
            if (!job->classification_label_missing[label_idx]) {
                job->classification_all_labels_missing = 0;
                break;
            }
        }
    }

    if (numericish && len > 0) {
        vals = (double *)malloc((size_t)len * sizeof(double));
        if (vals == NULL) {
            return 1;
        }
    }

    for (ptrdiff_t j = 0; j < len; j++) {
        int is_invalid = 0;
        double val = 0.0;
        const char *str_val = NULL;

        if (job->type == DDIWR_STRING) {
            str_val = job->str_data[j];
            is_invalid = (str_val == NULL);
        } else if (job->type == DDIWR_DOUBLE) {
            val = job->real_data[j];
            is_invalid = isnan(val);
        } else if (job->type == DDIWR_INTEGER) {
            int iv = job->int_data[j];
            is_invalid = (iv == INT_MIN);
            val = (double)iv;
        } else if (job->type == DDIWR_LOGICAL) {
            int lv = job->lgl_data[j];
            is_invalid = (lv == INT_MIN);
            val = (double)lv;
        } else {
            is_invalid = 1;
        }

        if (job->include_projection && !is_invalid) {
            job->source_has_observed = 1;

            if (job->source_numeric_candidate && job->type == DDIWR_STRING) {
                char *endptr = NULL;

                (void)strtod(str_val, &endptr);
                if (endptr == str_val || *endptr != '\0') {
                    job->source_numeric_candidate = 0;
                }
            }
        }

        if (job->has_labels && job->cat_count > 0) {
            for (int cat_i = 0; cat_i < job->cat_count; cat_i++) {
                if (c_value_matches_label(val, str_val, job, cat_i)) {
                    job->cat_freq[cat_i] += 1.0;
                    break;
                }
            }
        }

        if (!is_invalid && (c_value_in_na_values(val, str_val, job) || c_value_in_na_range(val, job))) {
            is_invalid = 1;
        }

        if (is_invalid) {
            invalid_n++;
            continue;
        }

        valid_obs++;
        if (job->include_projection) {
            job->has_observed_value = 1;
        }

        if (job->include_projection && job->classification_distinct_count < 15) {
            int seen = 0;

            if (job->type == DDIWR_STRING) {
                for (int d = 0; d < job->classification_distinct_count; d++) {
                    if (strcmp(distinct_string[d], str_val) == 0) {
                        seen = 1;
                        break;
                    }
                }

                if (!seen) {
                    distinct_string[job->classification_distinct_count++] = str_val;
                }
            }
            else {
                for (int d = 0; d < job->classification_distinct_count; d++) {
                    if (distinct_numeric[d] == val) {
                        seen = 1;
                        break;
                    }
                }

                if (!seen) {
                    distinct_numeric[job->classification_distinct_count++] = val;
                }
            }
        }

        if (job->include_projection && job->classification_label_count > 0) {
            int labelled = 0;

            for (int label_idx = 0; label_idx < job->classification_label_count; label_idx++) {
                if (c_value_matches_classification_label(val, str_val, job, label_idx)) {
                    labelled = 1;
                    break;
                }
            }

            if (!labelled) {
                job->classification_all_values_labelled = 0;
            }
        }

        if (!numericish) {
            continue;
        }

        if (job->type == DDIWR_STRING) {
            char *endptr = NULL;
            if (str_val == NULL) {
                numericish = 0;
                continue;
            }
            val = strtod(str_val, &endptr);
            if (endptr == str_val || *endptr != '\0') {
                numericish = 0;
                continue;
            }
        }

        if (job->include_projection && val < 0) {
            job->has_negative_value = 1;
        }

        vals[valid_n] = val;
        if (valid_n == 0) {
            minv = maxv = val;
            mean = val;
            m2 = 0.0;
        } else {
            if (val < minv) minv = val;
            if (val > maxv) maxv = val;
            double delta = val - mean;
            mean += delta / (double)(valid_n + 1);
            m2 += delta * (val - mean);
        }

        if (whole && (!isfinite(val) || fabs(val - nearbyint(val)) >= 1e-12)) {
            whole = 0;
        }

        int d_cnt = ddiwr_decimal_count(val);
        if (d_cnt > max_dcml) {
            max_dcml = d_cnt;
        }

        int width = c_display_width(val, job->type, str_val);
        if (width > max_width) {
            max_width = width;
        }

        if (!c_double_matches_label_value(val, job) && distinct_nonlabel_n < 5) {
            int seen = 0;
            for (int d = 0; d < distinct_nonlabel_n; d++) {
                if (distinct_nonlabel[d] == val) {
                    seen = 1;
                    break;
                }
            }
            if (!seen) {
                distinct_nonlabel[distinct_nonlabel_n++] = val;
            }
        }

        valid_n++;
    }

    job->sum_valid = (double)valid_obs;
    job->sum_invalid = (double)invalid_n;
    job->is_numericish = numericish;

    if (job->include_projection && !job->source_has_observed) {
        job->source_numeric_candidate = 0;
    }

    if (numericish && valid_n > 0) {
        job->max_dcml = max_dcml;
        job->max_width = max_width;
        job->whole = whole;

        if (!job->date_var && valid_n > 1) {
            job->val_min = minv;
            job->val_max = maxv;

            printnum = distinct_nonlabel_n > 4 || (valid_n > 2 && job->has_type_num);
            if (printnum) {
                double *median_work = (double *)malloc((size_t)valid_n * sizeof(double));
                double median = ddiwr_na_real();

                if (median_work == NULL) {
                    free(vals);
                    return 1;
                }
                {
                    memcpy(median_work, vals, (size_t)valid_n * sizeof(double));
                    qsort(median_work, (size_t)valid_n, sizeof(double), compare_doubles);
                    if ((valid_n % 2) == 1) {
                        median = median_work[valid_n / 2];
                    } else {
                        median = (median_work[valid_n / 2 - 1] + median_work[valid_n / 2]) / 2.0;
                    }
                    free(median_work);
                }

                job->stat_min = minv;
                job->stat_max = maxv;
                job->stat_mean = mean;
                job->stat_medn = median;
                if (valid_n > 1) {
                    job->stat_stdev = sqrt(m2 / ((double)valid_n - 1.0));
                } else {
                    job->stat_stdev = ddiwr_na_real();
                }
            } else {
                job->stat_min = ddiwr_na_real();
                job->stat_max = ddiwr_na_real();
                job->stat_mean = ddiwr_na_real();
                job->stat_medn = ddiwr_na_real();
                job->stat_stdev = ddiwr_na_real();
            }
        } else {
            job->val_min = ddiwr_na_real();
            job->val_max = ddiwr_na_real();
            job->stat_min = ddiwr_na_real();
            job->stat_max = ddiwr_na_real();
            job->stat_mean = ddiwr_na_real();
            job->stat_medn = ddiwr_na_real();
            job->stat_stdev = ddiwr_na_real();
        }
    } else {
        job->val_min = ddiwr_na_real();
        job->val_max = ddiwr_na_real();
        job->stat_min = ddiwr_na_real();
        job->stat_max = ddiwr_na_real();
        job->stat_mean = ddiwr_na_real();
        job->stat_medn = ddiwr_na_real();
        job->stat_stdev = ddiwr_na_real();
    }

    if (vals != NULL) {
        free(vals);
    }
    return 0;
}
