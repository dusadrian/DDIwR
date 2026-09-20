#include "metadata_text.h"
#include <stdlib.h>
#include <string.h>
#if !defined(_WIN32) && !defined(DDIWR_NO_THREADS) && \
    (!defined(__EMSCRIPTEN__) || defined(DDIWR_WASM_PTHREADS))
#include <pthread.h>
#define DDIWR_TEXT_PTHREADS 1
#endif

/* All replacements shrink, and each pass has gsub's nonrecursive semantics. */
static void replace(char *text, const char *pattern, const char *replacement) {
    size_t length = strlen(pattern);
    size_t replacement_length = strlen(replacement);
    char *read = text;
    char *write = text;
    while (*read) {
        if (strncmp(read, pattern, length) == 0) {
            memcpy(write, replacement, replacement_length);
            write += replacement_length;
            read += length;
        } else {
            *write++ = *read++;
        }
    }
    *write = '\0';
}

static int ascii_space(unsigned char value) {
    return value == ' ' || (value >= '\t' && value <= '\r');
}

static void remove_cdata(char *text) {
    char *read = text;
    char *write = text;
    while (*read) {
        if (strncmp(read, "<![CDATA[", 9) == 0) {
            read += 9;
        } else if (strncmp(read, "]]>", 3) == 0) {
            read += 3;
        } else {
            *write++ = *read++;
        }
    }
    *write = '\0';
}

int ddiwr_metadata_clean_text(const char *input, char **output) {
    *output = NULL;
    for (const unsigned char *p = (const unsigned char *)input; *p; p++) {
        if (*p >= 128) return 1;
    }
    char *text = malloc(strlen(input) + 1);
    if (text == NULL) return -1;
    strcpy(text, input);
    replace(text, "&amp;", "&");
    replace(text, "&lt;", "<");
    replace(text, "&gt;", ">");
    replace(text, "&apos;", "'");
    replace(text, "&quot;", "'");
    replace(text, "\"", "'");
    char *start = text;
    while (ascii_space((unsigned char)*start)) start++;
    size_t length = strlen(start);
    while (length && ascii_space((unsigned char)start[length - 1])) length--;
    memmove(text, start, length);
    text[length] = '\0';
    replace(text, "\\", "/");
    remove_cdata(text);
    replace(text, "`", "'");
    *output = text;
    return 0;
}

typedef struct {
    ddiwr_text_job *jobs;
    unsigned long count;
    unsigned long first;
    unsigned long stride;
} text_partition;

static void *process_partition(void *context) {
    text_partition *partition = context;
    for (unsigned long i = partition->first; i < partition->count; i += partition->stride) {
        ddiwr_text_job *job = &partition->jobs[i];
        job->status = 0;
        for (unsigned long j = 0; j < job->count; j++) {
            if (job->input[j] == NULL) continue;
            int status = ddiwr_metadata_clean_text(job->input[j], &job->output[j]);
            if (status != 0) {
                job->status = status;
                break;
            }
        }
    }
    return NULL;
}

int ddiwr_metadata_text_jobs(ddiwr_text_job *jobs, unsigned long count, int threads) {
    if (threads < 1) threads = 1;
    if (threads > 4) threads = 4;
    if ((unsigned long)threads > count) threads = (int)count;
    if (threads < 1) threads = 1;
#ifndef DDIWR_TEXT_PTHREADS
    threads = 1;
#endif
    text_partition partitions[4];
    for (int i = 0; i < threads; i++) {
        partitions[i] = (text_partition){jobs, count, (unsigned long)i, (unsigned long)threads};
    }
    int used = 1;
#ifdef DDIWR_TEXT_PTHREADS
    pthread_t handles[3];
    int started[3] = {0, 0, 0};
    for (int i = 1; i < threads; i++) {
#ifdef DDIWR_TEST_THREAD_LIMIT
        if (used - 1 >= DDIWR_TEST_THREAD_LIMIT) {
            process_partition(&partitions[i]);
            continue;
        }
#endif
        if (pthread_create(&handles[i - 1], NULL, process_partition, &partitions[i]) == 0) {
            started[i - 1] = 1;
            used++;
        } else {
            process_partition(&partitions[i]);
        }
    }
#endif
    process_partition(&partitions[0]);
#ifdef DDIWR_TEXT_PTHREADS
    for (int i = 1; i < threads; i++) {
        if (started[i - 1]) pthread_join(handles[i - 1], NULL);
    }
#endif
    return used;
}
