#include "variable_threads.h"

#include <stdlib.h>

#if defined(__EMSCRIPTEN__) || defined(DDIWR_NO_THREADS)
#define DDIWR_SERIAL_VARIABLES 1
#elif defined(_WIN32)
#include <windows.h>
#include <process.h>
#else
#include <pthread.h>
#include <unistd.h>
#endif

static int available_workers(void) {
#if defined(DDIWR_SERIAL_VARIABLES)
    return 1;
#elif defined(_WIN32)
    SYSTEM_INFO info;
    GetSystemInfo(&info);
    return info.dwNumberOfProcessors > 256 ? 256 : (int)info.dwNumberOfProcessors;
#else
    long count = sysconf(_SC_NPROCESSORS_ONLN);
    return count < 1 ? 1 : (count > 256 ? 256 : (int)count);
#endif
}

int ddiwr_variable_worker_count(size_t jobs, double values, int requested) {
    if (jobs < 2 || requested == 1 || (requested == 0 && values < 10000)) {
        return 1;
    }

    int count = available_workers();
    int limit = requested > 0 ? requested : 4;
    if (count > limit) {
        count = limit;
    }
    if ((size_t)count > jobs) {
        count = (int)jobs;
    }

    return count > 0 ? count : 1;
}

static void process_job(CVariableData *job, int operation) {
    job->error = 0;
    if (operation & DDIWR_ANALYSIS_STATS) {
        job->error = ddiwr_process_stats(job);
    }
    if ((operation & DDIWR_ANALYSIS_FORMATS) && job->len > 0 && !job->error) {
        ddiwr_infer_formats(job);
    }
}

#ifndef DDIWR_SERIAL_VARIABLES
typedef struct {
    CVariableData *jobs;
    size_t count;
    size_t next;
    int operation;
#ifdef _WIN32
    CRITICAL_SECTION mutex;
#else
    pthread_mutex_t mutex;
#endif
} VariableQueue;

static void drain_queue(VariableQueue *queue) {
    for (;;) {
#ifdef _WIN32
        EnterCriticalSection(&queue->mutex);
#else
        pthread_mutex_lock(&queue->mutex);
#endif
        size_t index = queue->next;
        if (index < queue->count) {
            queue->next++;
        }
#ifdef _WIN32
        LeaveCriticalSection(&queue->mutex);
#else
        pthread_mutex_unlock(&queue->mutex);
#endif
        if (index >= queue->count) {
            break;
        }
        process_job(&queue->jobs[index], queue->operation);
    }
}

#ifdef _WIN32
static unsigned __stdcall variable_worker(void *context) {
    drain_queue(context);
    return 0;
}
#else
static void *variable_worker(void *context) {
    drain_queue(context);
    return NULL;
}
#endif
#endif

int ddiwr_run_variable_jobs(CVariableData *jobs, size_t count, int requested, int operation) {
    double values = 0;
    for (size_t i = 0; i < count; i++) {
        values += (double)jobs[i].len;
    }
    int workers = ddiwr_variable_worker_count(count, values, requested);
    /* Format scans benefit from a lower default on the measured native workload. */
    if (requested == 0 && operation == DDIWR_ANALYSIS_FORMATS && workers > 2) {
        workers = 2;
    }
    int finished = 0;

#ifndef DDIWR_SERIAL_VARIABLES
    if (workers > 1) {
        VariableQueue queue = { .jobs = jobs, .count = count, .next = 0, .operation = operation };
#ifdef _WIN32
        HANDLE *threads = calloc((size_t)workers - 1, sizeof(HANDLE));
        int mutex_ready = threads && InitializeCriticalSectionEx(&queue.mutex, 0, 0);
#else
        pthread_t *threads = calloc((size_t)workers - 1, sizeof(pthread_t));
        int mutex_ready = threads && pthread_mutex_init(&queue.mutex, NULL) == 0;
#endif
        if (mutex_ready) {
            int started = 0;
            for (int i = 0; i < workers - 1; i++) {
                /* Compile-time failure injection for the standalone regression test. */
#ifdef DDIWR_TEST_THREAD_LIMIT
                if (started >= DDIWR_TEST_THREAD_LIMIT) {
                    break;
                }
#endif
#ifdef _WIN32
                threads[started] = (HANDLE)_beginthreadex(NULL, 0, variable_worker, &queue, 0, NULL);
                if (!threads[started]) {
                    break;
                }
#else
                if (pthread_create(&threads[started], NULL, variable_worker, &queue) != 0) {
                    break;
                }
#endif
                started++;
            }

            /* The caller participates and also completes jobs after startup failure. */
            drain_queue(&queue);
            for (int i = 0; i < started; i++) {
#ifdef _WIN32
                WaitForSingleObject(threads[i], INFINITE);
                CloseHandle(threads[i]);
#else
                pthread_join(threads[i], NULL);
#endif
            }
#ifdef _WIN32
            DeleteCriticalSection(&queue.mutex);
#else
            pthread_mutex_destroy(&queue.mutex);
#endif
            finished = 1;
        }
        free(threads);
    }
#else
    (void)workers;
#endif

    if (!finished) {
        for (size_t i = 0; i < count; i++) {
            process_job(&jobs[i], operation);
        }
    }

    for (size_t i = 0; i < count; i++) {
        if (jobs[i].error) {
            return jobs[i].error;
        }
    }
    return 0;
}
