#ifndef CPROC_H
#define CPROC_H

/* Proof of concept C dataflow processor units.  This works in
   conjunction with the epid / epid_app protocol used in exo_patch. */

/* FIXME: these are all synchronous machines with continous
   input/output.  Event driven machines need a different
   architecture. */

/* Use a naming scheme compatible with let.h */
#include "let.h"
#include <stdint.h>

/* The convention for now is that all state can be initialized to
   zero.  The update function follows the naming scheme in let.h All
   definitions go through a macro, to be able to add some bookkeeping
   data later. */
#define DEF_PROC(_proc_name, _config_var, _state_var, _input_var)     \
    static inline void _proc_name##_update(                             \
        const _proc_name##_config *_config_var,                         \
        _proc_name##_state *_state_var,                                 \
        const _proc_name##_input *_input_var)                           \


/* To simplify interfacing, all integers are machine words.  This
   should later be a parameter. */
typedef uint32_t w;

/* The hello world of state machines: the accumulator.
   The convention is to call the output "out".
   If there are more outputs, then use a struct.
   All other fields are supposed to be hidden state. */
typedef struct { w out; } acc_state;
typedef struct { w in;  } acc_input;
typedef void acc_config;
DEF_PROC(acc, c, s, i) {
    s->out += i->in;
} __attribute__((always_inline))

/* Edge detector. */
typedef struct { w out; w last; } edge_state ;
typedef struct { w in;          } edge_input;
typedef void edge_config;
DEF_PROC(edge, c, s, i) {
    s->out = (i->in != s->last);
    s->last = i->in;
} __attribute__((always_inline))


#endif
