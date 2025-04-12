#ifndef GRAPH_BASE_H
#define GRAPH_BASE_H

#include "osc.h"

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

    graph_base_process_fn process;
};


#endif
