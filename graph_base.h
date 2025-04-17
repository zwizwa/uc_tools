#ifndef GRAPH_BASE_H
#define GRAPH_BASE_H

#include "osc.h"

#ifndef LOG
#define LOG(...)
#endif

/* Base definitions for dataflow.lua generated graphs. */
struct graph_base;
typedef void  (*graph_base_process_fn)(struct graph_base *, uintptr_t);
struct graph_base {
    /* Every graph can be casted to an osc param_context. */
    struct param_context pc;

    /* Graphs need to have input and output defined.  Internally these
       are structs, but to the outside world they are also exposed as
       flat pointer arrays. */
    float **in;   uintptr_t nb_in;
    float **out;  uintptr_t nb_out;

    /* The main process method. */
    graph_base_process_fn process;

    /* The 2D buffer array is exposed for debugging. */
    float *buf;
    uint32_t nb_buf;
    uint32_t buf_size;
};

/* Connect any of the internal buffers to an output. */
static inline void graph_monitor(struct graph_base *b,
                                 uintptr_t buf_nb, uintptr_t out_nb) {
    if (buf_nb >= b->nb_buf) goto badarg;
    if (out_nb >= b->nb_out) goto badarg;
    float *buf = b->buf + b->buf_size * buf_nb;
    b->out[out_nb] = buf;
    return;
  badarg:
    LOG("graph_monitor: bad argument: buf_nb = %d, out_nb = %d\n",
        buf_nb, out_nb);
}


#endif
