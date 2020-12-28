#ifndef MOD_CPROC
#define MOD_CPROC

/* Proof of concept C dataflow processor units.  This works in
   conjunction with the epid / epid_app protocol used in exo_patch. */

/* FIXME: these are all synchronous machines with continous
   input/output.  Event driven machines need a different
   architecture. */

/* Use a naming scheme compatible with let.h */
#include "let.h"

/* The convention for now is that all state can be initialized to
   zero.  The update function follows the naming scheme in let.h All
   definitions go through a macro, to be able to add some bookkeeping
   data later. */
#define DEF_PROC(_proc_name, _state_var, ...) \
    static inline void _proc_name##_update(_proc_name##_t *_state_var, __VA_ARGS__)

/* To simplify interfacing, all integers are machine words.  This
   should later be a parameter. */
typedef uint32_t w;

/* The hello world of state machines: the accumulator.
   The convention is to call the output "out".
   If there are more outputs, then use a struct.
   All other fields are supposed to be hidden state. */
typedef struct { w out; } proc_acc_t;
DEF_PROC(proc_acc, s, w in) {
    s->out += in;
}

/* Edge detector. */
typedef struct { w out; w last; } proc_edge_t ;
DEF_PROC(proc_edge, s, w in) {
    s->out = (in != s->last);
    s->last = in;
}


#endif
