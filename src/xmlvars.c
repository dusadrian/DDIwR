#include <R.h>
#include <Rinternals.h>
#include <R_ext/Utils.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdarg.h>
#include <math.h>
#include <limits.h>

#ifndef _WIN32
#include <pthread.h>
#include <unistd.h>
#endif


typedef struct {
    char *buf;
    size_t len;
    size_t cap;
} ddiwr_strbuf;

typedef struct {
    SEXP x;
    SEXP classes_attr;
    SEXP label;
    SEXP measurement;
    SEXP labels;
    SEXP levels;
    SEXP na_values;
    SEXP na_range;
    SEXP xmlang;
    SEXP id;
    int factor_fallback;
    int include_formats;
    int is_date;
    char format_spss[64];
    char format_stata[64];
} xmlmeta_result;

static SEXP ddiwr_sym_labels = NULL;
static SEXP ddiwr_sym_label = NULL;
static SEXP ddiwr_sym_measurement = NULL;
static SEXP ddiwr_sym_na_values = NULL;
static SEXP ddiwr_sym_na_range = NULL;
static SEXP ddiwr_sym_xmlang = NULL;
static SEXP ddiwr_sym_id = NULL;

static void ddiwr_init_symbols(void) {
    if (ddiwr_sym_labels == NULL) {
        ddiwr_sym_labels = Rf_install("labels");
        ddiwr_sym_label = Rf_install("label");
        ddiwr_sym_measurement = Rf_install("measurement");
        ddiwr_sym_na_values = Rf_install("na_values");
        ddiwr_sym_na_range = Rf_install("na_range");
        ddiwr_sym_xmlang = Rf_install("xmlang");
        ddiwr_sym_id = Rf_install("ID");
    }
}


// C-level data structures and thread queues for data isolation
typedef struct {
    int index;
    int type;
    R_xlen_t len;
    int is_numericish;
    int has_type_num;
    int date_var;
    int has_labels;
    int cat_count;

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
    R_xlen_t *cat_label_idx;
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

typedef struct {
    CVariableData *jobs;
    R_xlen_t n_jobs;
    R_xlen_t next_job;
#ifndef _WIN32
    pthread_mutex_t mutex;
#endif
} CJobQueue;


// Forward declarations for R-dependent functions used in extraction
static int sexp_as_double(SEXP x, R_xlen_t i, double *out);
static int label_is_missing(SEXP labels, R_xlen_t j, SEXP na_values, SEXP na_range);
static int decimal_count(double x);
static SEXP getListElement(SEXP list, const char *name);

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
    if (job->type == STRSXP) {
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
    if (job->type == STRSXP || !job->has_num_na_range || isnan(val)) {
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
    if (job->type == STRSXP) {
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

static int c_display_width(double val, int type, const char *str_val) {
    char buf[128];
    if (type == REALSXP) {
        if (isnan(val)) return 0;
        snprintf(buf, sizeof(buf), "%.15g", val);
        return (int)strlen(buf);
    } else if (type == INTSXP) {
        if (isnan(val)) return 0;
        snprintf(buf, sizeof(buf), "%d", (int)val);
        return (int)strlen(buf);
    } else if (type == LGLSXP) {
        if (isnan(val)) return 0;
        return ((int)val) ? 4 : 5;
    } else if (type == STRSXP) {
        if (str_val == NULL) return 0;
        return (int)strlen(str_val);
    }
    return 0;
}

static void c_infer_formats(CVariableData *job) {
    int pN = 0;
    int allnax = 1;
    int nullabels = !job->has_labels;
    int decimals = 0;
    int numeric_width = 1;
    int maxvarchar = 0;
    R_xlen_t i = 0;

    job->is_date = 0;
    pN = (job->type != STRSXP);
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
        if (job->type == STRSXP) {
            if (job->str_data[i] != NULL) {
                allnax = 0;
                break;
            }
        } else if (job->type == REALSXP) {
            if (!isnan(job->real_data[i])) {
                allnax = 0;
                break;
            }
        } else if (job->type == INTSXP) {
            if (job->int_data[i] != INT_MIN) {
                allnax = 0;
                break;
            }
        } else if (job->type == LGLSXP) {
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
            
            if (job->type == REALSXP) {
                val = job->real_data[i];
                if (isnan(val)) continue;
            } else if (job->type == INTSXP) {
                int iv = job->int_data[i];
                if (iv == INT_MIN) continue;
                val = (double)iv;
            } else if (job->type == LGLSXP) {
                int lv = job->lgl_data[i];
                if (lv == INT_MIN) continue;
                val = (double)lv;
            }

            width = c_display_width(val, job->type, NULL);
            if (width > numeric_width) {
                numeric_width = width;
            }

            if (decimals < 3) {
                int d = decimal_count(val);
                if (d > decimals) {
                    decimals = d > 3 ? 3 : d;
                }
            }
        }
    }

    if (!pN && !allnax) {
        for (i = 0; i < job->len; i++) {
            int width = 0;
            if (job->type == STRSXP) {
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

static void process_stats_job(CVariableData *job) {
    R_xlen_t len = job->len;
    R_xlen_t valid_n = 0;
    R_xlen_t valid_obs = 0;
    R_xlen_t invalid_n = 0;
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

    if (numericish) {
        vals = (double *)malloc((size_t)len * sizeof(double));
        if (vals == NULL) {
            return;
        }
    }

    for (R_xlen_t j = 0; j < len; j++) {
        int is_invalid = 0;
        double val = 0.0;
        const char *str_val = NULL;

        if (job->type == STRSXP) {
            str_val = job->str_data[j];
            is_invalid = (str_val == NULL);
        } else if (job->type == REALSXP) {
            val = job->real_data[j];
            is_invalid = isnan(val);
        } else if (job->type == INTSXP) {
            int iv = job->int_data[j];
            is_invalid = (iv == INT_MIN);
            val = (double)iv;
        } else if (job->type == LGLSXP) {
            int lv = job->lgl_data[j];
            is_invalid = (lv == INT_MIN);
            val = (double)lv;
        } else {
            is_invalid = 1;
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

        if (!numericish) {
            continue;
        }

        if (job->type == STRSXP) {
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
        
        int d_cnt = decimal_count(val);
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
                double median = NA_REAL;

                if (median_work != NULL) {
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
                    job->stat_stdev = NA_REAL;
                }
            } else {
                job->stat_min = NA_REAL;
                job->stat_max = NA_REAL;
                job->stat_mean = NA_REAL;
                job->stat_medn = NA_REAL;
                job->stat_stdev = NA_REAL;
            }
        } else {
            job->val_min = NA_REAL;
            job->val_max = NA_REAL;
            job->stat_min = NA_REAL;
            job->stat_max = NA_REAL;
            job->stat_mean = NA_REAL;
            job->stat_medn = NA_REAL;
            job->stat_stdev = NA_REAL;
        }
    } else {
        job->val_min = NA_REAL;
        job->val_max = NA_REAL;
        job->stat_min = NA_REAL;
        job->stat_max = NA_REAL;
        job->stat_mean = NA_REAL;
        job->stat_medn = NA_REAL;
        job->stat_stdev = NA_REAL;
    }

    if (vals != NULL) {
        free(vals);
    }
}

static void extract_variable_data(
    SEXP data, SEXP variables, SEXP dates, R_xlen_t i, 
    R_xlen_t *cat_offsets, R_xlen_t **cat_label_idx_arr, int *cat_counts_arr,
    double *cat_freq_out, CVariableData *job
) {
    SEXP x = VECTOR_ELT(data, i);
    SEXP metadata = VECTOR_ELT(variables, i);
    SEXP labels = getListElement(metadata, "labels");
    SEXP na_values = getListElement(metadata, "na_values");
    SEXP na_range = getListElement(metadata, "na_range");
    SEXP type = getListElement(metadata, "type");

    job->index = (int)i;
    job->type = TYPEOF(x);
    job->len = XLENGTH(x);
    job->is_numericish = (TYPEOF(x) == REALSXP || TYPEOF(x) == INTSXP || TYPEOF(x) == LGLSXP || TYPEOF(x) == STRSXP);
    job->date_var = LOGICAL(dates)[i] == TRUE;
    job->has_labels = (labels != R_NilValue);
    job->cat_count = cat_counts_arr[i];

    job->has_type_num = 0;
    if (type != R_NilValue && TYPEOF(type) == STRSXP && XLENGTH(type) > 0) {
        const char *ct = CHAR(STRING_ELT(type, 0));
        if (strstr(ct, "num") != NULL) {
            job->has_type_num = 1;
        }
    }

    // Assign data pointers
    job->real_data = NULL;
    job->int_data = NULL;
    job->lgl_data = NULL;
    job->str_data = NULL;

    if (TYPEOF(x) == REALSXP) {
        job->real_data = REAL(x);
    } else if (TYPEOF(x) == INTSXP) {
        job->int_data = INTEGER(x);
    } else if (TYPEOF(x) == LGLSXP) {
        job->lgl_data = LOGICAL(x);
    } else if (TYPEOF(x) == STRSXP) {
        job->str_data = (const char **)malloc((size_t)job->len * sizeof(char *));
        for (R_xlen_t j = 0; j < job->len; j++) {
            if (STRING_ELT(x, j) == NA_STRING) {
                job->str_data[j] = NULL;
            } else {
                job->str_data[j] = CHAR(STRING_ELT(x, j));
            }
        }
    }

    // Extract missing values
    job->num_na_values_n = 0;
    job->has_num_na_range = 0;
    job->str_na_values_n = 0;

    if (na_values != R_NilValue && XLENGTH(na_values) > 0) {
        if (TYPEOF(x) == STRSXP && TYPEOF(na_values) == STRSXP) {
            job->str_na_values_n = (int)XLENGTH(na_values);
            if (job->str_na_values_n > 3) job->str_na_values_n = 3;
            for (int k = 0; k < job->str_na_values_n; k++) {
                if (STRING_ELT(na_values, k) == NA_STRING) {
                    job->str_na_values[k] = NULL;
                } else {
                    job->str_na_values[k] = CHAR(STRING_ELT(na_values, k));
                }
            }
        } else {
            int n_vals = (int)XLENGTH(na_values);
            if (n_vals > 3) n_vals = 3;
            for (int k = 0; k < n_vals; k++) {
                double val = 0.0;
                if (sexp_as_double(na_values, k, &val)) {
                    job->num_na_values[job->num_na_values_n++] = val;
                }
            }
        }
    }

    if (na_range != R_NilValue && XLENGTH(na_range) == 2) {
        job->has_num_na_range = 1;
        job->num_na_range[0] = REAL(na_range)[0];
        job->num_na_range[1] = REAL(na_range)[1];
    }

    // Extract category labels
    job->cat_label_dvals = NULL;
    job->cat_label_svals = NULL;
    job->cat_label_idx = NULL;
    job->cat_missing = NULL;
    job->cat_freq = NULL;

    if (job->has_labels && job->cat_count > 0) {
        job->cat_label_idx = (R_xlen_t *)malloc((size_t)job->cat_count * sizeof(R_xlen_t));
        memcpy(job->cat_label_idx, cat_label_idx_arr[i], (size_t)job->cat_count * sizeof(R_xlen_t));

        if (TYPEOF(labels) == STRSXP) {
            job->cat_label_svals = (const char **)malloc((size_t)job->cat_count * sizeof(char *));
            for (int k = 0; k < job->cat_count; k++) {
                R_xlen_t idx = job->cat_label_idx[k];
                if (STRING_ELT(labels, idx) == NA_STRING) {
                    job->cat_label_svals[k] = NULL;
                } else {
                    job->cat_label_svals[k] = CHAR(STRING_ELT(labels, idx));
                }
            }
        } else {
            job->cat_label_dvals = (double *)malloc((size_t)job->cat_count * sizeof(double));
            for (int k = 0; k < job->cat_count; k++) {
                R_xlen_t idx = job->cat_label_idx[k];
                double val = 0.0;
                if (sexp_as_double(labels, idx, &val)) {
                    job->cat_label_dvals[k] = val;
                } else {
                    job->cat_label_dvals[k] = NA_REAL;
                }
            }
        }

        job->cat_missing = (int *)malloc((size_t)job->cat_count * sizeof(int));
        for (int k = 0; k < job->cat_count; k++) {
            R_xlen_t idx = job->cat_label_idx[k];
            job->cat_missing[k] = label_is_missing(labels, idx, na_values, na_range);
        }

        job->cat_freq = &cat_freq_out[cat_offsets[i]];
        for (int k = 0; k < job->cat_count; k++) {
            job->cat_freq[k] = 0.0;
        }
    }
}

#ifndef _WIN32
static int xmlstats_available_threads(void) {
    long nproc = sysconf(_SC_NPROCESSORS_ONLN);
    if (nproc < 1) {
        nproc = 1;
    }
    if (nproc > INT_MAX) {
        nproc = INT_MAX;
    }
    return (int)nproc;
}

static void *xmlstats_worker_thread_main(void *arg) {
    CJobQueue *queue = (CJobQueue *)arg;
    for (;;) {
        R_xlen_t job_idx = -1;
        pthread_mutex_lock(&queue->mutex);
        if (queue->next_job < queue->n_jobs) {
            job_idx = queue->next_job++;
        }
        pthread_mutex_unlock(&queue->mutex);

        if (job_idx < 0) {
            break;
        }

        process_stats_job(&queue->jobs[job_idx]);
    }
    return NULL;
}

static void *xmlmeta_worker_thread_main(void *arg) {
    CJobQueue *queue = (CJobQueue *)arg;
    for (;;) {
        R_xlen_t job_idx = -1;
        pthread_mutex_lock(&queue->mutex);
        if (queue->next_job < queue->n_jobs) {
            job_idx = queue->next_job++;
        }
        pthread_mutex_unlock(&queue->mutex);

        if (job_idx < 0) {
            break;
        }

        CVariableData *job = &queue->jobs[job_idx];
        if (job->len > 0) {
            c_infer_formats(job);
        }
    }
    return NULL;
}
#endif

static SEXP getListElement(SEXP list, const char *name) {
    SEXP names = getAttrib(list, R_NamesSymbol);
    R_xlen_t i = 0;
    if (TYPEOF(list) != VECSXP || TYPEOF(names) != STRSXP) {
        return R_NilValue;
    }
    for (i = 0; i < XLENGTH(list); i++) {
        if (strcmp(CHAR(STRING_ELT(names, i)), name) == 0) {
            return VECTOR_ELT(list, i);
        }
    }
    return R_NilValue;
}

static int is_whole_double(double x) {
    if (!R_finite(x)) {
        return 0;
    }
    return fabs(x - nearbyint(x)) < 1e-12;
}

static int class_has(SEXP classes, const char *target) {
    R_xlen_t i = 0;

    if (TYPEOF(classes) != STRSXP) {
        return 0;
    }

    for (i = 0; i < XLENGTH(classes); i++) {
        if (STRING_ELT(classes, i) != NA_STRING &&
            strcmp(CHAR(STRING_ELT(classes, i)), target) == 0) {
            return 1;
        }
    }

    return 0;
}


static int decimal_count(double x) {
    char buf[128];
    char *dot = NULL;
    char *end = NULL;

    if (!R_finite(x) || is_whole_double(x)) {
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

static int parse_string_double(SEXP x, R_xlen_t i, double *out) {
    const char *s = NULL;
    char *end = NULL;
    double val = 0.0;

    if (STRING_ELT(x, i) == NA_STRING) {
        return 0;
    }

    s = CHAR(STRING_ELT(x, i));
    val = strtod(s, &end);
    if (end == s || *end != '\0') {
        return 0;
    }

    *out = val;
    return 1;
}

static int vector_possible_numeric(SEXP x) {
    R_xlen_t i = 0;
    double tmp = 0.0;

    if (x == R_NilValue) {
        return 1;
    }

    switch(TYPEOF(x)) {
        case REALSXP:
        case INTSXP:
        case LGLSXP:
            return 1;
        case STRSXP:
            for (i = 0; i < XLENGTH(x); i++) {
                if (STRING_ELT(x, i) == NA_STRING) {
                    continue;
                }
                if (!parse_string_double(x, i, &tmp)) {
                    return 0;
                }
            }
            return 1;
        default:
            return 0;
    }
}

static int sexp_as_double(SEXP x, R_xlen_t i, double *out) {
    switch(TYPEOF(x)) {
        case REALSXP:
            *out = REAL(x)[i];
            return !ISNAN(*out);
        case INTSXP:
            if (INTEGER(x)[i] == NA_INTEGER) {
                return 0;
            }
            *out = (double)INTEGER(x)[i];
            return 1;
        case LGLSXP:
            if (LOGICAL(x)[i] == NA_LOGICAL) {
                return 0;
            }
            *out = (double)LOGICAL(x)[i];
            return 1;
        case STRSXP:
            return parse_string_double(x, i, out);
        default:
            return 0;
    }
}

static int string_width_sexp(SEXP x, R_xlen_t i) {
    if (TYPEOF(x) != STRSXP || STRING_ELT(x, i) == NA_STRING) {
        return 0;
    }

    return (int)strlen(CHAR(STRING_ELT(x, i)));
}

static int display_width_sexp(SEXP x, R_xlen_t i) {
    char buf[128];

    switch(TYPEOF(x)) {
        case REALSXP:
            if (ISNAN(REAL(x)[i])) {
                return 0;
            }
            snprintf(buf, sizeof(buf), "%.15g", REAL(x)[i]);
            return (int)strlen(buf);
        case INTSXP:
            if (INTEGER(x)[i] == NA_INTEGER) {
                return 0;
            }
            snprintf(buf, sizeof(buf), "%d", INTEGER(x)[i]);
            return (int)strlen(buf);
        case LGLSXP:
            if (LOGICAL(x)[i] == NA_LOGICAL) {
                return 0;
            }
            return LOGICAL(x)[i] ? 4 : 5;
        case STRSXP:
            return string_width_sexp(x, i);
        default:
            return 0;
    }
}

static void infer_formats(SEXP x, SEXP classes, SEXP labels, char *spss, size_t spss_sz, char *stata, size_t stata_sz, int *is_date) {
    int pN = 0;
    int allnax = 1;
    int nullabels = labels == R_NilValue;
    int decimals = 0;
    int numeric_width = 1;
    int maxvarchar = 0;
    R_xlen_t i = 0;

    *is_date = 0;

    if (class_has(classes, "POSIXct")) {
        snprintf(spss, spss_sz, "DATETIME");
        snprintf(stata, stata_sz, "%%tc");
        return;
    }

    if (class_has(classes, "Date")) {
        *is_date = 1;
        spss[0] = '\0';
        stata[0] = '\0';
        return;
    }

    if (class_has(classes, "hms")) {
        snprintf(spss, spss_sz, "TIME");
        snprintf(stata, stata_sz, "%%tc");
        return;
    }

    pN = (TYPEOF(x) != STRSXP) && vector_possible_numeric(x);
    if (!nullabels) {
        pN = pN && vector_possible_numeric(labels);
    }

    for (i = 0; i < XLENGTH(x); i++) {
        if (TYPEOF(x) == STRSXP) {
            if (STRING_ELT(x, i) != NA_STRING) {
                allnax = 0;
                break;
            }
        }
        else {
            double tmp = 0.0;
            if (sexp_as_double(x, i, &tmp)) {
                allnax = 0;
                break;
            }
        }
    }

    if (pN && !allnax) {
        for (i = 0; i < XLENGTH(x); i++) {
            double val = 0.0;
            int width = 0;

            if (!sexp_as_double(x, i, &val)) {
                continue;
            }

            width = display_width_sexp(x, i);
            if (width > numeric_width) {
                numeric_width = width;
            }

            if (decimals < 3) {
                int d = decimal_count(val);
                if (d > decimals) {
                    decimals = d > 3 ? 3 : d;
                }
            }
        }
    }

    if (!pN && !allnax) {
        for (i = 0; i < XLENGTH(x); i++) {
            int width = string_width_sexp(x, i);
            if (width > maxvarchar) {
                maxvarchar = width;
            }
        }
    }

    if (!nullabels && !pN) {
        for (i = 0; i < XLENGTH(labels); i++) {
            int width = string_width_sexp(labels, i);
            if (width > maxvarchar) {
                maxvarchar = width;
            }
        }
    }

    if (pN) {
        snprintf(spss, spss_sz, "F%d.%d", numeric_width, decimals);
        snprintf(stata, stata_sz, "%%%d.%dg", numeric_width, decimals);
    }
    else {
        int width = maxvarchar > 0 ? maxvarchar : 1;
        snprintf(spss, spss_sz, "A%d", width);
        snprintf(stata, stata_sz, "%%%ds", width);
    }
}

static SEXP sanitize_na_values(SEXP na_values) {
    SEXP out = R_NilValue;
    R_xlen_t i = 0;
    R_xlen_t n = 0;

    if (na_values == R_NilValue) {
        return R_NilValue;
    }

    switch(TYPEOF(na_values)) {
        case REALSXP:
            for (i = 0; i < XLENGTH(na_values); i++) {
                if (!ISNAN(REAL(na_values)[i])) {
                    n++;
                }
            }
            if (n == 0) {
                return R_NilValue;
            }
            PROTECT(out = allocVector(REALSXP, n));
            n = 0;
            for (i = 0; i < XLENGTH(na_values); i++) {
                if (!ISNAN(REAL(na_values)[i])) {
                    REAL(out)[n++] = REAL(na_values)[i];
                }
            }
            UNPROTECT(1);
            return out;
        case INTSXP:
        case LGLSXP:
            for (i = 0; i < XLENGTH(na_values); i++) {
                int val = INTEGER(na_values)[i];
                if (val != NA_INTEGER) {
                    n++;
                }
            }
            if (n == 0) {
                return R_NilValue;
            }
            PROTECT(out = allocVector(TYPEOF(na_values), n));
            n = 0;
            for (i = 0; i < XLENGTH(na_values); i++) {
                int val = INTEGER(na_values)[i];
                if (val != NA_INTEGER) {
                    INTEGER(out)[n++] = val;
                }
            }
            UNPROTECT(1);
            return out;
        case STRSXP:
            for (i = 0; i < XLENGTH(na_values); i++) {
                if (STRING_ELT(na_values, i) != NA_STRING) {
                    n++;
                }
            }
            if (n == 0) {
                return R_NilValue;
            }
            PROTECT(out = allocVector(STRSXP, n));
            n = 0;
            for (i = 0; i < XLENGTH(na_values); i++) {
                if (STRING_ELT(na_values, i) != NA_STRING) {
                    SET_STRING_ELT(out, n++, STRING_ELT(na_values, i));
                }
            }
            UNPROTECT(1);
            return out;
        default:
            return na_values;
    }
}

static void xmlmeta_process_variable(SEXP data, xmlmeta_result *results, R_xlen_t i, int include_formats) {
    SEXP x = VECTOR_ELT(data, i);
    SEXP classes = getAttrib(x, R_ClassSymbol);
    SEXP labels = getAttrib(x, ddiwr_sym_labels);
    SEXP levels = getAttrib(x, R_LevelsSymbol);

    results[i].x = x;
    results[i].classes_attr = classes;
    results[i].label = getAttrib(x, ddiwr_sym_label);
    results[i].measurement = getAttrib(x, ddiwr_sym_measurement);
    results[i].labels = labels;
    results[i].levels = R_NilValue;
    results[i].na_values = getAttrib(x, ddiwr_sym_na_values);
    results[i].na_range = getAttrib(x, ddiwr_sym_na_range);
    results[i].xmlang = getAttrib(x, ddiwr_sym_xmlang);
    results[i].id = getAttrib(x, ddiwr_sym_id);
    results[i].factor_fallback = 0;
    results[i].include_formats = include_formats;
    results[i].is_date = 0;
    results[i].format_spss[0] = '\0';
    results[i].format_stata[0] = '\0';

    if (labels == R_NilValue && class_has(classes, "factor") && TYPEOF(levels) == STRSXP) {
        results[i].factor_fallback = 1;
        results[i].levels = levels;
    }

    if (include_formats) {
        infer_formats(
            x,
            classes,
            labels,
            results[i].format_spss,
            sizeof(results[i].format_spss),
            results[i].format_stata,
            sizeof(results[i].format_stata),
            &results[i].is_date
        );
    }
}



static int char_equal_sexp(SEXP x, R_xlen_t i, SEXP labels, R_xlen_t j) {
    if (TYPEOF(x) != STRSXP || TYPEOF(labels) != STRSXP) {
        return 0;
    }
    if (STRING_ELT(x, i) == NA_STRING || STRING_ELT(labels, j) == NA_STRING) {
        return 0;
    }
    return strcmp(CHAR(STRING_ELT(x, i)), CHAR(STRING_ELT(labels, j))) == 0;
}

static int value_in_na_values(SEXP x, R_xlen_t i, SEXP na_values) {
    R_xlen_t j = 0;
    double xnum = 0.0;
    double nnum = 0.0;

    if (na_values == R_NilValue || XLENGTH(na_values) == 0) {
        return 0;
    }

    if (TYPEOF(x) == STRSXP && TYPEOF(na_values) == STRSXP) {
        for (j = 0; j < XLENGTH(na_values); j++) {
            if (char_equal_sexp(x, i, na_values, j)) {
                return 1;
            }
        }
        return 0;
    }

    if (!sexp_as_double(x, i, &xnum)) {
        return 0;
    }

    for (j = 0; j < XLENGTH(na_values); j++) {
        if (sexp_as_double(na_values, j, &nnum) && xnum == nnum) {
            return 1;
        }
    }

    return 0;
}

static int value_in_na_range(SEXP x, R_xlen_t i, SEXP na_range) {
    double xnum = 0.0;
    double lo = 0.0;
    double hi = 0.0;

    if (na_range == R_NilValue || XLENGTH(na_range) < 2) {
        return 0;
    }

    if (!sexp_as_double(x, i, &xnum)) {
        return 0;
    }

    lo = REAL(na_range)[0];
    hi = REAL(na_range)[1];

    if (R_NegInf == lo) {
        return xnum <= hi;
    }
    if (R_PosInf == hi) {
        return xnum >= lo;
    }
    return xnum >= lo && xnum <= hi;
}

static int label_is_missing(SEXP labels, R_xlen_t j, SEXP na_values, SEXP na_range) {
    if (labels == R_NilValue || XLENGTH(labels) <= j) {
        return 0;
    }

    if (TYPEOF(labels) == STRSXP && TYPEOF(na_values) == STRSXP) {
        R_xlen_t k = 0;
        for (k = 0; k < XLENGTH(na_values); k++) {
            if (char_equal_sexp(labels, j, na_values, k)) {
                return 1;
            }
        }
        return 0;
    }

    if (value_in_na_values(labels, j, na_values)) {
        return 1;
    }
    return value_in_na_range(labels, j, na_range);
}

static int value_matches_label(SEXP x, R_xlen_t i, SEXP labels, R_xlen_t j) {
    double xnum = 0.0;
    double lnum = 0.0;

    if (TYPEOF(x) == STRSXP && TYPEOF(labels) == STRSXP) {
        return char_equal_sexp(x, i, labels, j);
    }

    if (!sexp_as_double(x, i, &xnum) || !sexp_as_double(labels, j, &lnum)) {
        return 0;
    }

    return xnum == lnum;
}




SEXP collect_datadscr_stats(SEXP data, SEXP variables, SEXP dates) {
    R_xlen_t n = 0;
    R_xlen_t i = 0;
    R_xlen_t cat_total = 0;
    SEXP out = R_NilValue;
    SEXP names = R_NilValue;
    SEXP var_dcml = R_NilValue;
    SEXP var_width = R_NilValue;
    SEXP range_units = R_NilValue;
    SEXP val_min = R_NilValue;
    SEXP val_max = R_NilValue;
    SEXP stat_min = R_NilValue;
    SEXP stat_max = R_NilValue;
    SEXP stat_mean = R_NilValue;
    SEXP stat_medn = R_NilValue;
    SEXP stat_stdev = R_NilValue;
    SEXP sum_valid = R_NilValue;
    SEXP sum_invalid = R_NilValue;
    SEXP cat_counts = R_NilValue;
    SEXP cat_values = R_NilValue;
    SEXP cat_labels = R_NilValue;
    SEXP cat_missing = R_NilValue;
    SEXP cat_freq = R_NilValue;
    R_xlen_t *cat_offsets = NULL;
    R_xlen_t **cat_label_idx_arr = NULL;
    int *cat_counts_arr = NULL;

    if (!Rf_isNewList(data) || !Rf_isNewList(variables)) {
        Rf_error("Arguments 'data' and 'variables' must be lists.");
    }

    n = XLENGTH(data);
    if (XLENGTH(variables) != n || !Rf_isLogical(dates) || XLENGTH(dates) != n) {
        Rf_error("Arguments 'variables' and 'dates' must have same length as 'data'.");
    }

    for (i = 0; i < n; i++) {
        SEXP metadata = VECTOR_ELT(variables, i);
        SEXP labels = getListElement(metadata, "labels");
        SEXP label_names = (labels == R_NilValue) ? R_NilValue : getAttrib(labels, R_NamesSymbol);
        R_xlen_t j = 0;
        if (labels == R_NilValue || TYPEOF(label_names) != STRSXP) {
            continue;
        }
        for (j = 0; j < XLENGTH(labels); j++) {
            if (STRING_ELT(label_names, j) != NA_STRING && strlen(CHAR(STRING_ELT(label_names, j))) > 0) {
                cat_total++;
            }
        }
    }

    PROTECT(out = allocVector(VECSXP, 17));
    PROTECT(names = allocVector(STRSXP, 17));
    PROTECT(var_dcml = allocVector(REALSXP, n));
    PROTECT(var_width = allocVector(REALSXP, n));
    PROTECT(range_units = allocVector(STRSXP, n));
    PROTECT(val_min = allocVector(REALSXP, n));
    PROTECT(val_max = allocVector(REALSXP, n));
    PROTECT(stat_min = allocVector(REALSXP, n));
    PROTECT(stat_max = allocVector(REALSXP, n));
    PROTECT(stat_mean = allocVector(REALSXP, n));
    PROTECT(stat_medn = allocVector(REALSXP, n));
    PROTECT(stat_stdev = allocVector(REALSXP, n));
    PROTECT(sum_valid = allocVector(REALSXP, n));
    PROTECT(sum_invalid = allocVector(REALSXP, n));
    PROTECT(cat_counts = allocVector(INTSXP, n));
    PROTECT(cat_values = allocVector(STRSXP, cat_total));
    PROTECT(cat_labels = allocVector(STRSXP, cat_total));
    PROTECT(cat_missing = allocVector(LGLSXP, cat_total));
    PROTECT(cat_freq = allocVector(REALSXP, cat_total));

    cat_offsets = (R_xlen_t *)calloc((size_t)n, sizeof(R_xlen_t));
    cat_label_idx_arr = (R_xlen_t **)calloc((size_t)n, sizeof(R_xlen_t *));
    cat_counts_arr = (int *)calloc((size_t)n, sizeof(int));
    if (cat_offsets == NULL || cat_label_idx_arr == NULL || cat_counts_arr == NULL) {
        free(cat_offsets);
        free(cat_label_idx_arr);
        free(cat_counts_arr);
        UNPROTECT(19);
        Rf_error("Failed to allocate category metadata buffers.");
    }

    for (i = 0; i < n; i++) {
        REAL(var_dcml)[i] = NA_REAL;
        REAL(var_width)[i] = NA_REAL;
        SET_STRING_ELT(range_units, i, mkChar("REAL"));
        REAL(val_min)[i] = NA_REAL;
        REAL(val_max)[i] = NA_REAL;
        REAL(stat_min)[i] = NA_REAL;
        REAL(stat_max)[i] = NA_REAL;
        REAL(stat_mean)[i] = NA_REAL;
        REAL(stat_medn)[i] = NA_REAL;
        REAL(stat_stdev)[i] = NA_REAL;
        REAL(sum_valid)[i] = NA_REAL;
        REAL(sum_invalid)[i] = NA_REAL;
        INTEGER(cat_counts)[i] = 0;
    }

    for (i = 0; i < n; i++) {
        SEXP metadata = VECTOR_ELT(variables, i);
        SEXP labels = getListElement(metadata, "labels");
        SEXP label_names = (labels == R_NilValue) ? R_NilValue : getAttrib(labels, R_NamesSymbol);
        SEXP na_values = getListElement(metadata, "na_values");
        SEXP na_range = getListElement(metadata, "na_range");
        R_xlen_t j = 0;
        int has_labels = labels != R_NilValue && TYPEOF(label_names) == STRSXP;
        int cat_count = 0;

        cat_offsets[i] = (i == 0) ? 0 : (cat_offsets[i - 1] + (R_xlen_t)cat_counts_arr[i - 1]);

        if (has_labels) {
            R_xlen_t *cat_label_idx = (R_xlen_t *)calloc((size_t)XLENGTH(labels), sizeof(R_xlen_t));
            if (cat_label_idx == NULL) {
                R_xlen_t k = 0;
                for (k = 0; k < i; k++) {
                    free(cat_label_idx_arr[k]);
                }
                free(cat_offsets);
                free(cat_label_idx_arr);
                free(cat_counts_arr);
                UNPROTECT(19);
                Rf_error("Failed to allocate category index buffer.");
            }
            cat_label_idx_arr[i] = cat_label_idx;
            for (j = 0; j < XLENGTH(labels); j++) {
                if (STRING_ELT(label_names, j) != NA_STRING && strlen(CHAR(STRING_ELT(label_names, j))) > 0) {
                    R_xlen_t pos = cat_offsets[i] + cat_count;

                    cat_label_idx[cat_count] = j;
                    if (TYPEOF(labels) == STRSXP) {
                        if (STRING_ELT(labels, j) == NA_STRING) {
                            SET_STRING_ELT(cat_values, pos, NA_STRING);
                        } else {
                            SET_STRING_ELT(cat_values, pos, STRING_ELT(labels, j));
                        }
                    } else {
                        char buf[128];
                        double lnum = 0.0;
                        if (sexp_as_double(labels, j, &lnum)) {
                            snprintf(buf, sizeof(buf), "%.15g", lnum);
                            SET_STRING_ELT(cat_values, pos, mkChar(buf));
                        } else {
                            SET_STRING_ELT(cat_values, pos, NA_STRING);
                        }
                    }
                    SET_STRING_ELT(cat_labels, pos, STRING_ELT(label_names, j));
                    LOGICAL(cat_missing)[pos] = label_is_missing(labels, j, na_values, na_range);
                    REAL(cat_freq)[pos] = 0.0;
                    cat_count++;
                }
            }
            INTEGER(cat_counts)[i] = cat_count;
            cat_counts_arr[i] = cat_count;
        }
    }

    CVariableData *jobs = (CVariableData *)calloc((size_t)n, sizeof(CVariableData));
    if (jobs == NULL) {
        for (i = 0; i < n; i++) {
            free(cat_label_idx_arr[i]);
        }
        free(cat_offsets);
        free(cat_label_idx_arr);
        free(cat_counts_arr);
        UNPROTECT(19);
        Rf_error("Failed to allocate C statistics jobs.");
    }

    for (i = 0; i < n; i++) {
        extract_variable_data(
            data, variables, dates, i,
            cat_offsets, cat_label_idx_arr, cat_counts_arr,
            REAL(cat_freq), &jobs[i]
        );
    }

#ifndef _WIN32
    int nworkers = xmlstats_available_threads();
    if (nworkers > 1 && n > 1) {
        pthread_t *threads = (pthread_t *)calloc((size_t)nworkers, sizeof(pthread_t));
        CJobQueue queue;
        
        if (threads == NULL) {
            for (i = 0; i < n; i++) {
                if (jobs[i].str_data != NULL) free(jobs[i].str_data);
                if (jobs[i].cat_label_idx != NULL) free(jobs[i].cat_label_idx);
                if (jobs[i].cat_label_svals != NULL) free(jobs[i].cat_label_svals);
                if (jobs[i].cat_label_dvals != NULL) free(jobs[i].cat_label_dvals);
                if (jobs[i].cat_missing != NULL) free(jobs[i].cat_missing);
            }
            free(jobs);
            for (i = 0; i < n; i++) {
                free(cat_label_idx_arr[i]);
            }
            free(cat_offsets);
            free(cat_label_idx_arr);
            free(cat_counts_arr);
            UNPROTECT(19);
            Rf_error("Failed to allocate stats worker threads.");
        }

        queue.jobs = jobs;
        queue.n_jobs = n;
        queue.next_job = 0;
        pthread_mutex_init(&queue.mutex, NULL);

        for (int t = 0; t < nworkers; t++) {
            pthread_create(&threads[t], NULL, xmlstats_worker_thread_main, &queue);
        }
        for (int t = 0; t < nworkers; t++) {
            pthread_join(threads[t], NULL);
        }
        pthread_mutex_destroy(&queue.mutex);
        free(threads);
    } else {
        for (i = 0; i < n; i++) {
            process_stats_job(&jobs[i]);
        }
    }
#else
    for (i = 0; i < n; i++) {
        process_stats_job(&jobs[i]);
    }
#endif

    for (i = 0; i < n; i++) {
        CVariableData *job = &jobs[i];
        REAL(sum_valid)[i] = job->sum_valid;
        REAL(sum_invalid)[i] = job->sum_invalid;

        if (job->is_numericish && job->sum_valid > 0) {
            REAL(var_dcml)[i] = (double)job->max_dcml;
            REAL(var_width)[i] = (double)job->max_width;
            SET_STRING_ELT(range_units, i, mkChar(job->whole ? "INT" : "REAL"));

            if (!job->date_var && (job->sum_valid - job->sum_invalid) > 0) {
                REAL(val_min)[i] = job->val_min;
                REAL(val_max)[i] = job->val_max;
                REAL(stat_min)[i] = job->stat_min;
                REAL(stat_max)[i] = job->stat_max;
                REAL(stat_mean)[i] = job->stat_mean;
                REAL(stat_medn)[i] = job->stat_medn;
                REAL(stat_stdev)[i] = job->stat_stdev;
            }
        }
    }

    for (i = 0; i < n; i++) {
        if (jobs[i].str_data != NULL) free(jobs[i].str_data);
        if (jobs[i].cat_label_idx != NULL) free(jobs[i].cat_label_idx);
        if (jobs[i].cat_label_svals != NULL) free(jobs[i].cat_label_svals);
        if (jobs[i].cat_label_dvals != NULL) free(jobs[i].cat_label_dvals);
        if (jobs[i].cat_missing != NULL) free(jobs[i].cat_missing);
    }
    free(jobs);

    SET_VECTOR_ELT(out, 0, var_dcml);
    SET_VECTOR_ELT(out, 1, var_width);
    SET_VECTOR_ELT(out, 2, range_units);
    SET_VECTOR_ELT(out, 3, val_min);
    SET_VECTOR_ELT(out, 4, val_max);
    SET_VECTOR_ELT(out, 5, stat_min);
    SET_VECTOR_ELT(out, 6, stat_max);
    SET_VECTOR_ELT(out, 7, stat_mean);
    SET_VECTOR_ELT(out, 8, stat_medn);
    SET_VECTOR_ELT(out, 9, stat_stdev);
    SET_VECTOR_ELT(out, 10, sum_valid);
    SET_VECTOR_ELT(out, 11, sum_invalid);
    SET_VECTOR_ELT(out, 12, cat_counts);
    SET_VECTOR_ELT(out, 13, cat_values);
    SET_VECTOR_ELT(out, 14, cat_labels);
    SET_VECTOR_ELT(out, 15, cat_missing);
    SET_VECTOR_ELT(out, 16, cat_freq);
    SET_STRING_ELT(names, 0, mkChar("var_dcml"));
    SET_STRING_ELT(names, 1, mkChar("var_width"));
    SET_STRING_ELT(names, 2, mkChar("range_units"));
    SET_STRING_ELT(names, 3, mkChar("val_min"));
    SET_STRING_ELT(names, 4, mkChar("val_max"));
    SET_STRING_ELT(names, 5, mkChar("stat_min"));
    SET_STRING_ELT(names, 6, mkChar("stat_max"));
    SET_STRING_ELT(names, 7, mkChar("stat_mean"));
    SET_STRING_ELT(names, 8, mkChar("stat_medn"));
    SET_STRING_ELT(names, 9, mkChar("stat_stdev"));
    SET_STRING_ELT(names, 10, mkChar("sum_valid"));
    SET_STRING_ELT(names, 11, mkChar("sum_invalid"));
    SET_STRING_ELT(names, 12, mkChar("cat_counts"));
    SET_STRING_ELT(names, 13, mkChar("cat_values"));
    SET_STRING_ELT(names, 14, mkChar("cat_labels"));
    SET_STRING_ELT(names, 15, mkChar("cat_missing"));
    SET_STRING_ELT(names, 16, mkChar("cat_freq"));
    setAttrib(out, R_NamesSymbol, names);

    for (i = 0; i < n; i++) {
        free(cat_label_idx_arr[i]);
    }
    free(cat_offsets);
    free(cat_label_idx_arr);
    free(cat_counts_arr);

    UNPROTECT(19);
    return out;
}

SEXP collect_xml_metadata(SEXP data, SEXP include_formats) {
    R_xlen_t i = 0;
    R_xlen_t n = 0;
    SEXP out = R_NilValue;
    SEXP out_names = R_NilValue;
    xmlmeta_result *results = NULL;
    int do_formats = 1;

    if (!Rf_isNewList(data)) {
        Rf_error("Argument 'data' must be a list.");
    }
    if (!Rf_isLogical(include_formats) || XLENGTH(include_formats) != 1) {
        Rf_error("Argument 'include_formats' must be a logical scalar.");
    }
    do_formats = LOGICAL(include_formats)[0] != 0;

    ddiwr_init_symbols();

    n = XLENGTH(data);
    results = (xmlmeta_result *)calloc((size_t)n, sizeof(xmlmeta_result));
    if (results == NULL) {
        Rf_error("Failed to allocate metadata buffers.");
    }

    for (i = 0; i < n; i++) {
        xmlmeta_process_variable(data, results, i, 0); // Extract attributes, skip format inference sequentially
    }

    if (do_formats) {
        CVariableData *jobs = (CVariableData *)calloc((size_t)n, sizeof(CVariableData));
        if (jobs == NULL) {
            free(results);
            Rf_error("Failed to allocate format inference jobs.");
        }

        for (i = 0; i < n; i++) {
            SEXP x = results[i].x;
            SEXP classes = results[i].classes_attr;
            SEXP labels = results[i].labels;

            jobs[i].index = (int)i;
            jobs[i].type = TYPEOF(x);
            jobs[i].len = XLENGTH(x);
            jobs[i].has_labels = (labels != R_NilValue);
            jobs[i].cat_count = labels != R_NilValue ? (int)XLENGTH(labels) : 0;
            jobs[i].is_numericish = (TYPEOF(x) == REALSXP || TYPEOF(x) == INTSXP || TYPEOF(x) == LGLSXP || TYPEOF(x) == STRSXP);
            jobs[i].date_var = 0;

            jobs[i].real_data = NULL;
            jobs[i].int_data = NULL;
            jobs[i].lgl_data = NULL;
            jobs[i].str_data = NULL;

            if (TYPEOF(x) == REALSXP) {
                jobs[i].real_data = REAL(x);
            } else if (TYPEOF(x) == INTSXP) {
                jobs[i].int_data = INTEGER(x);
            } else if (TYPEOF(x) == LGLSXP) {
                jobs[i].lgl_data = LOGICAL(x);
            } else if (TYPEOF(x) == STRSXP) {
                jobs[i].str_data = (const char **)malloc((size_t)jobs[i].len * sizeof(char *));
                for (R_xlen_t j = 0; j < jobs[i].len; j++) {
                    if (STRING_ELT(x, j) == NA_STRING) {
                        jobs[i].str_data[j] = NULL;
                    } else {
                        jobs[i].str_data[j] = CHAR(STRING_ELT(x, j));
                    }
                }
            }

            jobs[i].cat_label_dvals = NULL;
            jobs[i].cat_label_svals = NULL;
            if (labels != R_NilValue && jobs[i].cat_count > 0) {
                if (TYPEOF(labels) == STRSXP) {
                    jobs[i].cat_label_svals = (const char **)malloc((size_t)jobs[i].cat_count * sizeof(char *));
                    for (int k = 0; k < jobs[i].cat_count; k++) {
                        if (STRING_ELT(labels, k) == NA_STRING) {
                            jobs[i].cat_label_svals[k] = NULL;
                        } else {
                            jobs[i].cat_label_svals[k] = CHAR(STRING_ELT(labels, k));
                        }
                    }
                } else {
                    jobs[i].cat_label_dvals = (double *)malloc((size_t)jobs[i].cat_count * sizeof(double));
                    for (int k = 0; k < jobs[i].cat_count; k++) {
                        double val = 0.0;
                        if (sexp_as_double(labels, k, &val)) {
                            jobs[i].cat_label_dvals[k] = val;
                        } else {
                            jobs[i].cat_label_dvals[k] = NA_REAL;
                        }
                    }
                }
            }

            if (class_has(classes, "POSIXct")) {
                snprintf(jobs[i].format_spss, sizeof(jobs[i].format_spss), "DATETIME");
                snprintf(jobs[i].format_stata, sizeof(jobs[i].format_stata), "%%tc");
                jobs[i].is_date = 0;
                jobs[i].len = 0;
            } else if (class_has(classes, "Date")) {
                jobs[i].is_date = 1;
                jobs[i].format_spss[0] = '\0';
                jobs[i].format_stata[0] = '\0';
                jobs[i].len = 0;
            } else if (class_has(classes, "hms")) {
                snprintf(jobs[i].format_spss, sizeof(jobs[i].format_spss), "TIME");
                snprintf(jobs[i].format_stata, sizeof(jobs[i].format_stata), "%%tc");
                jobs[i].is_date = 0;
                jobs[i].len = 0;
            }
        }

#ifndef _WIN32
        int nworkers = xmlstats_available_threads();
        if (nworkers > 1 && n > 1) {
            pthread_t *threads = (pthread_t *)calloc((size_t)nworkers, sizeof(pthread_t));
            CJobQueue queue;
            
            if (threads == NULL) {
                for (i = 0; i < n; i++) {
                    if (jobs[i].str_data != NULL) free(jobs[i].str_data);
                    if (jobs[i].cat_label_svals != NULL) free(jobs[i].cat_label_svals);
                    if (jobs[i].cat_label_dvals != NULL) free(jobs[i].cat_label_dvals);
                }
                free(jobs);
                free(results);
                Rf_error("Failed to allocate format worker threads.");
            }

            queue.jobs = jobs;
            queue.n_jobs = n;
            queue.next_job = 0;
            pthread_mutex_init(&queue.mutex, NULL);

            for (int t = 0; t < nworkers; t++) {
                pthread_create(&threads[t], NULL, xmlmeta_worker_thread_main, &queue);
            }
            for (int t = 0; t < nworkers; t++) {
                pthread_join(threads[t], NULL);
            }
            pthread_mutex_destroy(&queue.mutex);
            free(threads);
        } else {
            for (i = 0; i < n; i++) {
                if (jobs[i].len > 0) {
                    c_infer_formats(&jobs[i]);
                }
            }
        }
#else
        for (i = 0; i < n; i++) {
            if (jobs[i].len > 0) {
                c_infer_formats(&jobs[i]);
            }
        }
#endif

        for (i = 0; i < n; i++) {
            strcpy(results[i].format_spss, jobs[i].format_spss);
            strcpy(results[i].format_stata, jobs[i].format_stata);
            results[i].is_date = jobs[i].is_date;

            if (jobs[i].str_data != NULL) free(jobs[i].str_data);
            if (jobs[i].cat_label_svals != NULL) free(jobs[i].cat_label_svals);
            if (jobs[i].cat_label_dvals != NULL) free(jobs[i].cat_label_dvals);
        }
        free(jobs);
    }

    PROTECT(out = allocVector(VECSXP, n));
    PROTECT(out_names = getAttrib(data, R_NamesSymbol));

    for (i = 0; i < n; i++) {
        SEXP item = R_NilValue;
        SEXP item_names = R_NilValue;
        SEXP classes = results[i].classes_attr;
        int idx = 0;
        int fields = do_formats ? 5 : 4; /* classes, na_range, [varFormat], xmlang, ID */
        int has_label = results[i].label != R_NilValue;
        int has_measurement = results[i].measurement != R_NilValue;
        int has_labels = results[i].labels != R_NilValue || results[i].factor_fallback;
        int has_na_values = results[i].na_values != R_NilValue;

        if (has_label) fields++;
        if (has_measurement) fields++;
        if (has_labels) fields++;
        if (has_na_values) fields++;

        PROTECT(item = allocVector(VECSXP, fields));
        PROTECT(item_names = allocVector(STRSXP, fields));

        if (classes == R_NilValue) {
            PROTECT(classes = allocVector(STRSXP, 1));
            SET_STRING_ELT(classes, 0, mkChar(type2char(TYPEOF(results[i].x))));
        }
        else {
            PROTECT(classes);
        }
        SET_VECTOR_ELT(item, idx, classes);
        SET_STRING_ELT(item_names, idx++, mkChar("classes"));

        if (has_label) {
            SET_VECTOR_ELT(item, idx, results[i].label);
            SET_STRING_ELT(item_names, idx++, mkChar("label"));
        }

        if (has_measurement) {
            SET_VECTOR_ELT(item, idx, results[i].measurement);
            SET_STRING_ELT(item_names, idx++, mkChar("measurement"));
        }

        if (has_labels) {
            SEXP labels = results[i].labels;
            if (results[i].factor_fallback) {
                R_xlen_t k = XLENGTH(results[i].levels);
                SEXP fac_labels = PROTECT(allocVector(INTSXP, k));
                SEXP fac_names = PROTECT(allocVector(STRSXP, k));
                R_xlen_t j = 0;

                for (j = 0; j < k; j++) {
                    INTEGER(fac_labels)[j] = (int)(j + 1);
                    SET_STRING_ELT(fac_names, j, STRING_ELT(results[i].levels, j));
                }
                setAttrib(fac_labels, R_NamesSymbol, fac_names);
                labels = fac_labels;
            }
            SET_VECTOR_ELT(item, idx, labels);
            SET_STRING_ELT(item_names, idx++, mkChar("labels"));
            if (results[i].factor_fallback) {
                UNPROTECT(2);
            }
        }

        if (has_na_values) {
            SEXP na_values = PROTECT(sanitize_na_values(results[i].na_values));
            if (na_values != R_NilValue) {
                SET_VECTOR_ELT(item, idx, na_values);
                SET_STRING_ELT(item_names, idx++, mkChar("na_values"));
            }
            UNPROTECT(1);
        }

        SET_VECTOR_ELT(item, idx, results[i].na_range);
        SET_STRING_ELT(item_names, idx++, mkChar("na_range"));

        if (do_formats) {
            if (results[i].is_date) {
                SEXP fmt = PROTECT(mkString("date"));
                SET_VECTOR_ELT(item, idx, fmt);
                UNPROTECT(1);
            }
            else {
                SEXP fmt = PROTECT(allocVector(STRSXP, 2));
                SET_STRING_ELT(fmt, 0, mkChar(results[i].format_spss));
                SET_STRING_ELT(fmt, 1, mkChar(results[i].format_stata));
                SET_VECTOR_ELT(item, idx, fmt);
                UNPROTECT(1);
            }
            SET_STRING_ELT(item_names, idx++, mkChar("varFormat"));
        }

        SET_VECTOR_ELT(item, idx, results[i].xmlang);
        SET_STRING_ELT(item_names, idx++, mkChar("xmlang"));

        SET_VECTOR_ELT(item, idx, results[i].id);
        SET_STRING_ELT(item_names, idx++, mkChar("ID"));

        setAttrib(item, R_NamesSymbol, item_names);
        SET_VECTOR_ELT(out, i, item);
        UNPROTECT(3);
    }

    if (TYPEOF(out_names) == STRSXP && XLENGTH(out_names) == n) {
        setAttrib(out, R_NamesSymbol, out_names);
    }

    free(results);
    UNPROTECT(2);
    return out;
}

SEXP label_freqs(SEXP x, SEXP labels, SEXP wt) {
    R_xlen_t n = XLENGTH(x);
    R_xlen_t k = XLENGTH(labels);
    R_xlen_t i = 0;
    R_xlen_t j = 0;
    SEXP out = R_NilValue;
    int weighted = wt != R_NilValue && wt != R_NilValue && TYPEOF(wt) != NILSXP;

    if (!(TYPEOF(x) == REALSXP || TYPEOF(x) == INTSXP || TYPEOF(x) == LGLSXP || TYPEOF(x) == STRSXP)) {
        Rf_error("Argument 'x' must be an atomic vector.");
    }
    if (!(TYPEOF(labels) == REALSXP || TYPEOF(labels) == INTSXP || TYPEOF(labels) == LGLSXP || TYPEOF(labels) == STRSXP)) {
        Rf_error("Argument 'labels' must be an atomic vector.");
    }
    if (weighted && XLENGTH(wt) != n) {
        Rf_error("Argument 'wt' must have same length as 'x'.");
    }

    PROTECT(out = allocVector(REALSXP, k));
    for (j = 0; j < k; j++) {
        REAL(out)[j] = 0.0;
    }

    for (i = 0; i < n; i++) {
        int is_missing = 0;
        double w = 1.0;

        if (TYPEOF(x) == STRSXP) {
            is_missing = (STRING_ELT(x, i) == NA_STRING);
        } else if (TYPEOF(x) == REALSXP) {
            is_missing = ISNAN(REAL(x)[i]);
        } else if (TYPEOF(x) == INTSXP) {
            is_missing = INTEGER(x)[i] == NA_INTEGER;
        } else if (TYPEOF(x) == LGLSXP) {
            is_missing = LOGICAL(x)[i] == NA_LOGICAL;
        }

        if (is_missing) {
            continue;
        }

        if (weighted) {
            if (TYPEOF(wt) == REALSXP) {
                if (ISNAN(REAL(wt)[i])) {
                    continue;
                }
                w = REAL(wt)[i];
            } else if (TYPEOF(wt) == INTSXP) {
                if (INTEGER(wt)[i] == NA_INTEGER) {
                    continue;
                }
                w = (double)INTEGER(wt)[i];
            } else if (TYPEOF(wt) == LGLSXP) {
                if (LOGICAL(wt)[i] == NA_LOGICAL) {
                    continue;
                }
                w = (double)LOGICAL(wt)[i];
            } else {
                double tmp = 0.0;
                if (!sexp_as_double(wt, i, &tmp)) {
                    continue;
                }
                w = tmp;
            }
        }

        for (j = 0; j < k; j++) {
            if (value_matches_label(x, i, labels, j)) {
                REAL(out)[j] += w;
                break;
            }
        }
    }

    UNPROTECT(1);
    return out;
}

static void sb_init(ddiwr_strbuf *sb, size_t initial_cap) {
    sb->len = 0;
    sb->cap = initial_cap > 0 ? initial_cap : 1024;
    sb->buf = (char *)malloc(sb->cap);
    if (sb->buf == NULL) {
        Rf_error("Out of memory while allocating XML buffer.");
    }
    sb->buf[0] = '\0';
}

static void sb_free(ddiwr_strbuf *sb) {
    if (sb->buf != NULL) {
        free(sb->buf);
        sb->buf = NULL;
    }
    sb->len = 0;
    sb->cap = 0;
}

static void sb_reserve(ddiwr_strbuf *sb, size_t add) {
    size_t need = sb->len + add + 1;
    if (need <= sb->cap) {
        return;
    }
    while (sb->cap < need) {
        sb->cap *= 2;
    }
    sb->buf = (char *)realloc(sb->buf, sb->cap);
    if (sb->buf == NULL) {
        Rf_error("Out of memory while growing XML buffer.");
    }
}

static void sb_append(ddiwr_strbuf *sb, const char *s) {
    size_t n = strlen(s);
    sb_reserve(sb, n);
    memcpy(sb->buf + sb->len, s, n);
    sb->len += n;
    sb->buf[sb->len] = '\0';
}

static void sb_appendf(ddiwr_strbuf *sb, const char *fmt, ...) {
    va_list args;
    va_list args2;
    int needed = 0;

    va_start(args, fmt);
    va_copy(args2, args);
    needed = vsnprintf(NULL, 0, fmt, args);
    va_end(args);

    if (needed < 0) {
        va_end(args2);
        Rf_error("Failed formatting XML content.");
    }

    sb_reserve(sb, (size_t)needed);
    vsnprintf(sb->buf + sb->len, sb->cap - sb->len, fmt, args2);
    va_end(args2);
    sb->len += (size_t)needed;
}

static void sb_append_xml_escaped(ddiwr_strbuf *sb, const char *s) {
    const char *p = s;
    while (*p) {
        switch (*p) {
            case '&': sb_append(sb, "&amp;"); break;
            case '<': sb_append(sb, "&lt;"); break;
            case '>': sb_append(sb, "&gt;"); break;
            case '"': sb_append(sb, "&quot;"); break;
            case '\'': sb_append(sb, "&apos;"); break;
            default: {
                char c[2];
                c[0] = *p;
                c[1] = '\0';
                sb_append(sb, c);
            }
        }
        p++;
    }
}

static void sb_append_indent(ddiwr_strbuf *sb, int level, int indent_width) {
    int i = 0;
    int spaces = level * indent_width;
    if (spaces <= 0) {
        return;
    }
    sb_reserve(sb, (size_t)spaces);
    for (i = 0; i < spaces; i++) {
        sb->buf[sb->len++] = ' ';
    }
    sb->buf[sb->len] = '\0';
}

SEXP write_text_file(SEXP path, SEXP text) {
    FILE *fp = NULL;
    const char *cpath = NULL;
    size_t total_written = 0;
    size_t total_bytes = 0;
    R_xlen_t i = 0;

    if (!Rf_isString(path) || XLENGTH(path) != 1) {
        Rf_error("Argument 'path' must be a character scalar.");
    }

    if (!Rf_isString(text) || XLENGTH(text) < 1) {
        Rf_error("Argument 'text' must be a character vector.");
    }

    cpath = CHAR(STRING_ELT(path, 0));

    fp = fopen(cpath, "wb");
    if (fp == NULL) {
        Rf_error("Cannot open file for writing: %s", cpath);
    }

    for (i = 0; i < XLENGTH(text); i++) {
        const char *ctext = CHAR(STRING_ELT(text, i));
        size_t nbytes = strlen(ctext);
        size_t written = 0;

        total_bytes += nbytes;

        if (nbytes > 0) {
            written = fwrite(ctext, 1, nbytes, fp);
            total_written += written;
        }
    }

    if (fclose(fp) != 0) {
        Rf_error("Error while closing file: %s", cpath);
    }

    if (total_written != total_bytes) {
        Rf_error("Failed to write complete content to file: %s", cpath);
    }

    return R_NilValue;
}

SEXP make_datadscr_xml(
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
) {
    R_xlen_t i = 0;
    R_xlen_t n = 0;
    ddiwr_strbuf sb;
    SEXP out = R_NilValue;
    const char *nsp = NULL;
    int indent = 2;
    int level0 = 1;
    int level_var = 0;
    int level_var_child = 0;
    int level_var_grand = 0;

    if (!Rf_isString(ns_prefix) || XLENGTH(ns_prefix) != 1) {
        Rf_error("Argument 'ns_prefix' must be a character scalar.");
    }
    nsp = CHAR(STRING_ELT(ns_prefix, 0));

    if (!Rf_isInteger(indent_width) || XLENGTH(indent_width) != 1) {
        Rf_error("Argument 'indent_width' must be an integer scalar.");
    }
    if (!Rf_isInteger(base_level) || XLENGTH(base_level) != 1) {
        Rf_error("Argument 'base_level' must be an integer scalar.");
    }

    indent = INTEGER(indent_width)[0];
    level0 = INTEGER(base_level)[0];
    if (indent < 0 || level0 < 0) {
        Rf_error("Arguments 'indent_width' and 'base_level' must be non-negative.");
    }

    level_var = level0 + 1;
    level_var_child = level0 + 2;
    level_var_grand = level0 + 3;

    if (!Rf_isString(var_names)) {
        Rf_error("Argument 'var_names' must be a character vector.");
    }
    n = XLENGTH(var_names);

    if (!Rf_isString(var_ids) || XLENGTH(var_ids) != n) {
        Rf_error("Argument 'var_ids' must be a character vector with same length as 'var_names'.");
    }
    if (!Rf_isString(var_labels) || XLENGTH(var_labels) != n) {
        Rf_error("Argument 'var_labels' must be a character vector with same length as 'var_names'.");
    }
    if (!Rf_isReal(var_dcml) || XLENGTH(var_dcml) != n) {
        Rf_error("Argument 'var_dcml' must be a numeric vector with same length as 'var_names'.");
    }
    if (!Rf_isString(range_units) || XLENGTH(range_units) != n) {
        Rf_error("Argument 'range_units' must be a character vector with same length as 'var_names'.");
    }
    if (!Rf_isReal(val_min) || XLENGTH(val_min) != n) {
        Rf_error("Argument 'val_min' must be a numeric vector with same length as 'var_names'.");
    }
    if (!Rf_isReal(val_max) || XLENGTH(val_max) != n) {
        Rf_error("Argument 'val_max' must be a numeric vector with same length as 'var_names'.");
    }
    if (!Rf_isReal(inval_min) || XLENGTH(inval_min) != n) {
        Rf_error("Argument 'inval_min' must be a numeric vector with same length as 'var_names'.");
    }
    if (!Rf_isReal(inval_max) || XLENGTH(inval_max) != n) {
        Rf_error("Argument 'inval_max' must be a numeric vector with same length as 'var_names'.");
    }
    if (!Rf_isReal(stat_min) || XLENGTH(stat_min) != n) {
        Rf_error("Argument 'stat_min' must be a numeric vector with same length as 'var_names'.");
    }
    if (!Rf_isReal(stat_max) || XLENGTH(stat_max) != n) {
        Rf_error("Argument 'stat_max' must be a numeric vector with same length as 'var_names'.");
    }
    if (!Rf_isReal(stat_mean) || XLENGTH(stat_mean) != n) {
        Rf_error("Argument 'stat_mean' must be a numeric vector with same length as 'var_names'.");
    }
    if (!Rf_isReal(stat_medn) || XLENGTH(stat_medn) != n) {
        Rf_error("Argument 'stat_medn' must be a numeric vector with same length as 'var_names'.");
    }
    if (!Rf_isReal(stat_stdev) || XLENGTH(stat_stdev) != n) {
        Rf_error("Argument 'stat_stdev' must be a numeric vector with same length as 'var_names'.");
    }
    if (!Rf_isReal(sum_valid) || XLENGTH(sum_valid) != n) {
        Rf_error("Argument 'sum_valid' must be a numeric vector with same length as 'var_names'.");
    }
    if (!Rf_isReal(sum_invalid) || XLENGTH(sum_invalid) != n) {
        Rf_error("Argument 'sum_invalid' must be a numeric vector with same length as 'var_names'.");
    }
    if (!Rf_isString(varformat_type) || XLENGTH(varformat_type) != n) {
        Rf_error("Argument 'varformat_type' must be a character vector with same length as 'var_names'.");
    }
    if (!Rf_isString(varformat_value) || XLENGTH(varformat_value) != n) {
        Rf_error("Argument 'varformat_value' must be a character vector with same length as 'var_names'.");
    }
    if (!Rf_isInteger(cat_counts) || XLENGTH(cat_counts) != n) {
        Rf_error("Argument 'cat_counts' must be an integer vector with same length as 'var_names'.");
    }
    if (!Rf_isString(cat_values)) {
        Rf_error("Argument 'cat_values' must be a character vector.");
    }
    if (!Rf_isString(cat_labels)) {
        Rf_error("Argument 'cat_labels' must be a character vector.");
    }
    if (!Rf_isLogical(cat_missing)) {
        Rf_error("Argument 'cat_missing' must be a logical vector.");
    }
    if (!Rf_isReal(cat_freq)) {
        Rf_error("Argument 'cat_freq' must be a numeric vector.");
    }
    if (
        XLENGTH(cat_values) != XLENGTH(cat_labels) ||
        XLENGTH(cat_values) != XLENGTH(cat_missing) ||
        XLENGTH(cat_values) != XLENGTH(cat_freq)
    ) {
        Rf_error("Category vectors should have equal length.");
    }

    PROTECT(out = Rf_allocVector(STRSXP, n));

    R_xlen_t cat_offset = 0;

    for (i = 0; i < n; i++) {
        const char *vname = NULL;
        const char *vid = NULL;
        const char *vlab = NULL;
        const char *vunit = NULL;
        const char *vfmt_type = NULL;
        const char *vfmt_value = NULL;
        double vdcml = REAL(var_dcml)[i];
        double vmin = REAL(val_min)[i];
        double vmax = REAL(val_max)[i];
        double ivmin = REAL(inval_min)[i];
        double ivmax = REAL(inval_max)[i];
        double smin = REAL(stat_min)[i];
        double smax = REAL(stat_max)[i];
        double smean = REAL(stat_mean)[i];
        double smedn = REAL(stat_medn)[i];
        double sstdev = REAL(stat_stdev)[i];
        double sval = REAL(sum_valid)[i];
        double sinv = REAL(sum_invalid)[i];
        SEXP s_name = STRING_ELT(var_names, i);
        SEXP s_id = STRING_ELT(var_ids, i);
        SEXP s_lbl = STRING_ELT(var_labels, i);
        SEXP s_unit = STRING_ELT(range_units, i);
        SEXP s_vfmt_type = STRING_ELT(varformat_type, i);
        SEXP s_vfmt_value = STRING_ELT(varformat_value, i);
        int cat_n = INTEGER(cat_counts)[i];

        if (s_name == NA_STRING || s_id == NA_STRING) {
            UNPROTECT(1);
            Rf_error("Arguments 'var_names' and 'var_ids' should not contain NA.");
        }

        vname = CHAR(s_name);
        vid = CHAR(s_id);
        vlab = (s_lbl == NA_STRING) ? "" : CHAR(s_lbl);
        vunit = (s_unit == NA_STRING) ? "REAL" : CHAR(s_unit);
        vfmt_type = (s_vfmt_type == NA_STRING) ? "" : CHAR(s_vfmt_type);
        vfmt_value = (s_vfmt_value == NA_STRING) ? "" : CHAR(s_vfmt_value);

        sb_init(&sb, 1024);

        sb_append_indent(&sb, level_var, indent);
        sb_appendf(&sb, "<%svar", nsp);
        sb_append(&sb, " ID=\"");
        sb_append_xml_escaped(&sb, vid);
        sb_append(&sb, "\" name=\"");
        sb_append_xml_escaped(&sb, vname);
        if (R_FINITE(vdcml)) {
            sb_appendf(&sb, "\" dcml=\"%.0f", vdcml);
        }
        sb_append(&sb, "\">\n");

        if (strlen(vlab) > 0) {
            sb_append_indent(&sb, level_var_child, indent);
            sb_appendf(&sb, "<%slabl>", nsp);
            sb_append_xml_escaped(&sb, vlab);
            sb_appendf(&sb, "</%slabl>\n", nsp);
        }

        if (R_FINITE(vmin) && R_FINITE(vmax)) {
            sb_append_indent(&sb, level_var_child, indent);
            sb_appendf(&sb, "<%svalrng>\n", nsp);
            sb_append_indent(&sb, level_var_grand, indent);
            sb_appendf(&sb, "<%srange UNITS=\"%s\" min=\"%.15g\" max=\"%.15g\"/>\n", nsp, vunit, vmin, vmax);
            sb_append_indent(&sb, level_var_child, indent);
            sb_appendf(&sb, "</%svalrng>\n", nsp);
        }

        if (R_FINITE(ivmin) || R_FINITE(ivmax)) {
            sb_append_indent(&sb, level_var_child, indent);
            sb_appendf(&sb, "<%sinvalrng>\n", nsp);
            sb_append_indent(&sb, level_var_grand, indent);
            sb_appendf(&sb, "<%srange UNITS=\"%s\"", nsp, vunit);
            if (R_FINITE(ivmin)) {
                sb_appendf(&sb, " min=\"%.15g\"", ivmin);
            }
            if (R_FINITE(ivmax)) {
                sb_appendf(&sb, " max=\"%.15g\"", ivmax);
            }
            sb_append(&sb, "/>\n");
            sb_append_indent(&sb, level_var_child, indent);
            sb_appendf(&sb, "</%sinvalrng>\n", nsp);
        }

        if (R_FINITE(smin)) {
            sb_append_indent(&sb, level_var_child, indent);
            sb_appendf(
                &sb,
                "<%ssumStat type=\"min\">%.15g</%ssumStat>\n",
                nsp, smin, nsp
            );
        }

        if (R_FINITE(smax)) {
            sb_append_indent(&sb, level_var_child, indent);
            sb_appendf(
                &sb,
                "<%ssumStat type=\"max\">%.15g</%ssumStat>\n",
                nsp, smax, nsp
            );
        }

        if (R_FINITE(smean)) {
            sb_append_indent(&sb, level_var_child, indent);
            sb_appendf(
                &sb,
                "<%ssumStat type=\"mean\">%.15g</%ssumStat>\n",
                nsp, smean, nsp
            );
        }

        if (R_FINITE(smedn)) {
            sb_append_indent(&sb, level_var_child, indent);
            sb_appendf(
                &sb,
                "<%ssumStat type=\"medn\">%.15g</%ssumStat>\n",
                nsp, smedn, nsp
            );
        }

        if (R_FINITE(sstdev)) {
            sb_append_indent(&sb, level_var_child, indent);
            sb_appendf(
                &sb,
                "<%ssumStat type=\"stdev\">%.15g</%ssumStat>\n",
                nsp, sstdev, nsp
            );
        }

        if (R_FINITE(sval)) {
            sb_append_indent(&sb, level_var_child, indent);
            sb_appendf(
                &sb,
                "<%ssumStat type=\"vald\" wgtd=\"not-wgtd\">%.15g</%ssumStat>\n",
                nsp, sval, nsp
            );
        }

        if (R_FINITE(sinv)) {
            sb_append_indent(&sb, level_var_child, indent);
            sb_appendf(
                &sb,
                "<%ssumStat type=\"invd\" wgtd=\"not-wgtd\">%.15g</%ssumStat>\n",
                nsp, sinv, nsp
            );
        }

        if (cat_n < 0) {
            UNPROTECT(1);
            sb_free(&sb);
            Rf_error("Category counts should be non-negative.");
        }

        if (cat_offset + cat_n > XLENGTH(cat_values)) {
            UNPROTECT(1);
            sb_free(&sb);
            Rf_error("Category offsets exceed category vector lengths.");
        }

        for (int j = 0; j < cat_n; j++) {
            R_xlen_t idx = cat_offset + j;
            SEXP s_cat_val = STRING_ELT(cat_values, idx);
            SEXP s_cat_lab = STRING_ELT(cat_labels, idx);
            int ismiss = LOGICAL(cat_missing)[idx];
            double freq = REAL(cat_freq)[idx];
            const char *cval = (s_cat_val == NA_STRING) ? "" : CHAR(s_cat_val);
            const char *clab = (s_cat_lab == NA_STRING) ? "" : CHAR(s_cat_lab);

            sb_append_indent(&sb, level_var_child, indent);
            sb_appendf(
                &sb,
                "<%scatgry%s>\n",
                nsp,
                (ismiss == TRUE ? " missing=\"Y\"" : "")
            );

            sb_append_indent(&sb, level_var_grand, indent);
            sb_appendf(&sb, "<%scatValu>", nsp);
            sb_append_xml_escaped(&sb, cval);
            sb_appendf(&sb, "</%scatValu>\n", nsp);

            sb_append_indent(&sb, level_var_grand, indent);
            sb_appendf(&sb, "<%slabl>", nsp);
            sb_append_xml_escaped(&sb, clab);
            sb_appendf(&sb, "</%slabl>\n", nsp);

            if (R_FINITE(freq)) {
                sb_append_indent(&sb, level_var_grand, indent);
                sb_appendf(
                    &sb,
                    "<%scatStat type=\"freq\">%.15g</%scatStat>\n",
                    nsp, freq, nsp
                );
            }

            sb_append_indent(&sb, level_var_child, indent);
            sb_appendf(&sb, "</%scatgry>\n", nsp);
        }
        cat_offset += cat_n;

        if (strlen(vfmt_type) > 0 && strlen(vfmt_value) > 0) {
            sb_append_indent(&sb, level_var_child, indent);
            sb_appendf(
                &sb,
                "<%svarFormat type=\"%s\">",
                nsp, vfmt_type
            );
            sb_append_xml_escaped(&sb, vfmt_value);
            sb_appendf(&sb, "</%svarFormat>\n", nsp);
        }

        sb_append_indent(&sb, level_var, indent);
        sb_appendf(&sb, "</%svar>\n", nsp);

        SET_STRING_ELT(out, i, Rf_mkChar(sb.buf));
        sb_free(&sb);
    }

    UNPROTECT(1);
    return out;
}
