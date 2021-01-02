#ifndef CPROC_H
#define CPROC_H

/* Proof of concept C dataflow processor units.  This works in
   conjunction with the epid / epid_app protocol used in exo_patch,
   and code generation from epid_cproc.

   API will likely change to accomodate all use cases.

   Basic concepts:

   - provide a C language level macro PROC that can be used as code
     gen target, but is also manually usable.

   - processors have state, input, parameters, configuration

   - state is private to each instance

   - input is synchronous (think constant sample rate), or evented

   - support for subgraph execution, to implement evented systems. see LET_COND

   - const configuration is for specializing processors, e.g. bind to specific hw GPIO

   - dynamic parameters allow behavior change without causing events.
     this is likely only useful in synchronous systems.

*/


/* About static, dynamic parameters.

   These could probably be unified, but for good C code generation
   that will need some deeper abstraction, one that can split const
   and param per instance and generates cproc.h "classes" from a
   higher level specification.

   For now this distinction is fixed.  It is used mostly for hardware
   configuration.  If a different flavor is needed (e.g. moving from
   const to dynamic) then just create another processor type.  This is
   hard to design up front: actual use will show what is best.
*/



/* A-normal form for dataflow networks.

   Currently thinking to keep this _really_ simple:

   - don't separate signal output and state output structs

   - require named inputs

   - every instance has a const config type, which can be NULL

   - execution can be conditional, e.g. to implement evented systems
     as partial graph execution.


   Revisit?

   - require all init to be zero

*/

/* This is essentially "let", which applies a parameterized (curried)
   processor to a set of dataflow input signals. */
#define PROC_COND(_subgraph_cond,_instance_name,_type_name,_config_ptr,_param_ptr,...) \
    static _type_name##_state _instance_name = {};                      \
    if (_subgraph_cond) {                                               \
        const  _type_name##_input _instance_name##_input = { __VA_ARGS__ }; \
        _type_name##_update(&_instance_name, _config_ptr, _param_ptr, &_instance_name##_input); \
    }

/* Synchronous systems have subgraph condition disabled: full graph is executed every time. */
#define PROC(...) PROC_COND(1, __VA_ARGS__)


#include <stdint.h>

/* The convention for now is that all state can be initialized to
   zero.  The update function follows the naming scheme in let.h All
   definitions go through a macro, to be able to add some bookkeeping
   data later. */
#define DEF_PROC(_proc_name, _state_var, _config_var, _param_var, _input_var) \
    static inline void _proc_name##_update(                             \
        _proc_name##_state *_state_var,                                 \
        const _proc_name##_config *_config_var,                         \
        const _proc_name##_config *_param_var,                          \
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
typedef void acc_param;
DEF_PROC(acc, s, c, p, i) {
    s->out += i->in;
} __attribute__((always_inline))

/* Edge detector. */
typedef struct { w out; w last; } edge_state ;
typedef struct { w in;          } edge_input;
typedef void edge_config;
typedef void edge_param;
DEF_PROC(edge, s, c, p, i) {
    s->out = (i->in != s->last);
    s->last = i->in;
} __attribute__((always_inline))


#endif
