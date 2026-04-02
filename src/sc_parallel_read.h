#ifndef SC_PARALLEL_READ_H
#define SC_PARALLEL_READ_H

#include <R.h>
#include <Rinternals.h>

SEXP declared_df_parse_dta_file_parallel(
    SEXP spec,
    SEXP encoding,
    SEXP cols_skip,
    SEXP n_max,
    SEXP rows_skip,
    SEXP num_threads
);

#endif
