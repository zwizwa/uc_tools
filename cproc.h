#ifndef CPROC_H
#define CPROC_H

/* C dataflow processing API + some processors.

   Note that the idea here is mostly to define an API.  There are some
   processors defined, but ultimately it will be simpler to generate
   those from a more high level description.

   This code works in conjunction with the epid_cproc.erl code
   generator, which fits in the "universal patcher" framework: dynamic
   and static patching of distributed dataflow networks.

   There is a dynamic instantiator that re-uses the processor API
   (e.g. replaces static PROC macro with dynamic instantiation and
   connectivity).  See mod_bpmodular.c


   API will likely change to accomodate all use cases.

   Basic concepts:

   - provide a C language level macro PROC that can be used as code
     generation target (compling abstract ANF to concrete C, as a
     sequence of PROC binding statments)

   - keep the API sane so it is also usable for manual coding.

   - processors have private state, a dataflow output, many dataflow
     inputs, dynamic parameters, and static configuration.

   - input is synchronous (think constant sample rate), or evented

   - there is preliminary support for statically compiled subgraph
     execution, e.g. to implement evented systems. see PROC_COND and
     epid_cproc.erl

   - const configuration is for specializing processors, e.g. bind to
     specific hw GPIO.  it seems necessary to distinguish this from
     parameters.

   - dynamic parameters allow behavior change without causing events.
     this is likely only useful in synchronous systems.

*/


#include "metastruct.h"


/* PROC implements A-normal form (ANF) for dataflow networks.

   to keep this simple:

   - don't separate signal output and state output structs

   - require named inputs

   - every instance has an optional const config type, dynamic
     parmeter type.

   - execution can be conditional, e.g. to implement evented systems
     as partial graph execution.

   - state variables in this implementation initialize to zero.  use a
     separate parameter mechanism to initialize processors.

*/

/* This is essentially "let", which applies a parameterized (curried)
   processor to a set of dataflow input signals. */
#define PROC_COND(_subgraph_cond,_instance_name,_type_name,_config_ptr,_param_ptr,...) \
    static _type_name##_state _instance_name = {};                      \
    if (_subgraph_cond) {                                               \
        const _type_name##_input _instance_name##_input = { __VA_ARGS__ }; \
        _type_name##_update(&_instance_name, _config_ptr, _param_ptr, &_instance_name##_input); \
    }

/* Synchronous systems have subgraph condition disabled: full graph is
   executed every time. */
#define PROC(...) PROC_COND(1, __VA_ARGS__)


#include <stdint.h>


/* All definitions go through a macro, to be able to add some
   bookkeeping data later. */
#define DEF_PROC(_proc_name, _state_var, _config_var, _param_var, _input_var) \
    DEF_PROC_STRUCTS(_proc_name)                                        \
    static inline void _proc_name##_update(                             \
        _proc_name##_state *_state_var,                                 \
        const _proc_name##_config *_config_var,                         \
        const _proc_name##_param *_param_var,                    \
        const _proc_name##_input *_input_var)                    \

/* Datatypes are specified in a collection of macros, to make it
   easier to generate wrappers. */
#define DEF_PROC_STRUCTS(name) \
    STRUCT_DEF(name##_state, for_##name##_state); \
    STRUCT_DEF(name##_input, for_##name##_input); \
    STRUCT_DEF(name##_param, for_##name##_param); \
    STRUCT_CONST_DEF(name##_config, for_##name##_config); \


/* The struct field macros can also be reified to metadata. */
struct proc_meta {
    const char *name;
    const struct metastruct_struct state, input, param, config;
};
#define DEF_PROC_META(_name)                                  \
    METASTRUCT_DEF(_name##_state,  for_##_name##_state);      \
    METASTRUCT_DEF(_name##_input,  for_##_name##_input);      \
    METASTRUCT_DEF(_name##_param,  for_##_name##_param);      \
    METASTRUCT_DEF(_name##_config, for_##_name##_config);     \
    const struct proc_meta _name##_meta = {                   \
        .name   = #_name,                                    \
        .state  = METASTRUCT_STRUCT(_name##_state),          \
        .input  = METASTRUCT_STRUCT(_name##_input),          \
        .param  = METASTRUCT_STRUCT(_name##_param),          \
        .config = METASTRUCT_STRUCT(_name##_config),         \
    }


/* To simplify interfacing, all atomic values are machine words.  This
   is a property of the individual processors, and is not essential
   for the composition mechanism, i.e. the API. */
typedef uint32_t w;

/* The hello world of state machines: the accumulator.
   The convention is to call the output "out".
   If there are more outputs, then use a struct.
   All other fields are supposed to be hidden state. */
#define for_acc_state(m) m(w,out)
#define for_acc_input(m) m(w,in)
#define for_acc_config(m)
#define for_acc_param(m)
//DEF_PROC_STRUCTS(acc)

DEF_PROC(acc, s, c, p, i) {
    s->out += i->in;
} __attribute__((always_inline))

/* Edge detector. */
#define for_edge_state(m) m(w,out) m(w,last)
#define for_edge_input(m) m(w,in)
#define for_edge_config(m)
#define for_edge_param(m)

//DEF_STRUCTS(edge)
DEF_PROC(edge, s, c, p, i) {
    s->out = (i->in != s->last);
    s->last = i->in;
} __attribute__((always_inline))


#endif
