#ifndef DDIWR_VARIABLE_THREADS_H
#define DDIWR_VARIABLE_THREADS_H

#include "variable_analysis.h"

enum {
    DDIWR_ANALYSIS_STATS = 1,
    DDIWR_ANALYSIS_FORMATS = 2
};

int ddiwr_variable_worker_count(size_t jobs, double values, int requested);
int ddiwr_run_variable_jobs(CVariableData *jobs, size_t count, int requested, int operation);

#endif
