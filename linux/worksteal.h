#ifndef WORKSTEAL_H
#define WORKSTEAL_H

/* General design: first attempt is to use a shared data structure
   that contains the tasks and the queues.  It needs to fit in a
   static memory model so work with a bump allocator.  The task
   structure is known at compile time so the pool size is known at
   compile time.

   A task has a number of outputs.  These are modeled to the extent
   that dependencies need to be notified.  When a task is done, all
   its outputs are atomically notified, so the structure needs to
   contain an out->in reverse graph.

   TODO:

   Best to build this for a specific example: a partitioned
   convolution matrix filter with FFT and CMUL tasks.

*/

#include "macros.h"
#include "bump_alloc_aligned.h"
#include <stdint.h>

/* The static cost is just in bits so not much overhead caused so just
   use a machine word here. */
typedef uintptr_t wss_input_mask_t;
#define WSS_MAX_NB_INPUTS (8 * sizeof(wss_input_mask_t))

/* The input index needs to be able to represent a bit index into wss_input_mask_t */
typedef uint8_t wss_input_index_t;
CT_ASSERT(wss_input_t_size, (1<<(8*sizeof(wss_input_index_t))) >= WSS_MAX_NB_INPUTS);

/* 2^16 tasks seems enough for now. */
typedef uint16_t wss_task_index_t;
#define WSS_MAX_NB_TASKS (1<<(sizeof(wss_task_index_t)*8));

struct wss_output {
    wss_task_index_t task_index;
    wss_input_index_t input_index;
    uint8_t _reserved_;
};
CT_ASSERT(wss_output_size, sizeof(struct wss_output) == 4);

struct wss_task {

    /* Dependency graph: a done/waiting mask and corresponding task
       indices.  The nb_inputs does not need to be stored: it is
       implicit in the use of the bitmask (i.e. bits that do not
       correspond to an allocated input_task index slot are never
       used. */
    wss_input_mask_t input_mask;
    wss_task_index_t *input_task;

    /* Reverse dependency graph for notification contain a list of
       (task,input). */
    struct wss_output *output;

    uint8_t nb_outputs;
    uint8_t owner_index;
};

#define WSS_SCHEDULER_POOL_SIZE (16*1024)
struct wss_scheduler {
    uintptr_t pool[WSS_SCHEDULER_POOL_SIZE / sizeof(uintptr_t)];
    struct bump_alloc_aligned ba;
    struct wss_task *task; // Pool of all tasks
};

static inline void* wss_alloc(struct wss_scheduler *s, uintptr_t nb_bytes) {
    return bump_alloc_aligned_alloc(&s->ba, nb_bytes);
}
static inline void wss_init(struct wss_scheduler *s) {
    bump_alloc_aligned_init(&s->ba, s->pool, ARRAY_SIZE(s->pool));
}

// FIXME: Should probably be a lock per worker.
static inline void wss_lock(struct wss_scheduler *s) {
}
static inline void wss_unlock(struct wss_scheduler *s) {
}


/* Run-time (locked) operations. */

static inline void wss_notify_done(struct wss_scheduler *s,
                                   wss_task_index_t task_index) {
    wss_lock(s);
    struct wss_task *task = &s->task[task_index];
    for(uintptr_t i=0; i<task->nb_outputs; i++) {
        struct wss_output *o = &task->output[i];
        struct wss_task *dep_task = &s->task[o->task_index];
        dep_task->input_mask &= ~(1 << o->input_index);
        if (!dep_task->input_mask) {
            /* FIXME: Task is now runnable.  It should be moved to the
               runnable queue of the worker that owns it. */

            /* Note: I think that the non-runnable tasks do not need
               to be added to a queue. */
        }
    }
    wss_unlock(s);
}

// TODO: workers and queues
// how to sort queues?

#endif
