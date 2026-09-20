#ifndef DDIWR_METADATA_TEXT_H
#define DDIWR_METADATA_TEXT_H

/* No R dependencies. Input is read-only; caller owns output on success.
 * 0 = success, 1 = use locale-aware fallback, -1 = allocation failure. */
int ddiwr_metadata_clean_text(const char *input, char **output);

typedef struct {
    const char **input;
    char **output;
    unsigned long count;
    int status;
} ddiwr_text_job;

/* Joins all readers before returning. Returns actual participating thread count. */
int ddiwr_metadata_text_jobs(ddiwr_text_job *jobs, unsigned long count, int threads);

#endif
