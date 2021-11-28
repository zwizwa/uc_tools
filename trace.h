#ifndef TRACE_H
#define TRACE_H

/* Structured trace logging, API.

   The problem this tries to solve is to have a logging/tracing API
   that is easly converted from exisiting printf-style log messages,
   and that can be implemented either by print-style log messages, or
   by a binary serialization protocol.

   I've spent a lot of time on this in the past, never quite got to
   something that is easy to use at the caller end.  Eventually ended
   up at the idea of "writer context": a macro that creates a context
   in which values can be added to a trace log message by calling
   functions.

   The user can then define tags and types.

   Example:

        TRACE(some_tag, t) {
            t->some_int(123);
            t->some_obj(&obj);
        }

   - The entry point to the trace mechanism is a macro, since we need
     some symbol manipulation.

   - The only thing the macro does is to introduce scope: it will bind
     the variable t to a pointer to a stuct of trace_types, which are
     user defined serialization functions.

   - Not used (for now it's simpler to keep things flat), but possible
     in future: each tag could in theory have its own name space of
     trace types.

   - The tag preceeds the variable to make this easy to grep.  E.g. if
     you know the tag, the statement can be grepped as 'TRACE(some_tag'

   For implementation this file provides a number of macros to declare
   and define the structure types that create the lists of tags and
   types.

 */

#include <stdint.h>

/* API & MACRO IMPL */
struct trace_namespace;
extern const struct trace_namespace trace_namespace;

/* Create local 'ns' scope and execute pre/post functions by abusing a
   for(;;) construct and C expression syntax. */
#define TRACE_NS(ns,ns_tag,ns_types,...)                                \
    for(const struct trace_types *ns_types =                            \
            ({ns->base->begin(ns->tags->ns_tag); ns->types;});          \
        ns_types;                                                       \
        ns_types=({ns->base->end();(typeof(ns_types))0;}))

#define TRACE(ns_tag,ns_types,...)                              \
    TRACE_NS((&trace_namespace),ns_tag,ns_types,__VA_ARGS__)

struct trace_base {
    void (*begin)(const char *);
    void (*end)(void);
};
struct trace_tags;
struct trace_types;
struct trace_namespace {
    const struct trace_tags  *tags;
    const struct trace_types *types;
    const struct trace_base  *base;
};

typedef void (*trace_write_fn)(uintptr_t arg);

/* Macros for declaring structures and defining structure instances
   based on list macros FOR_TRACE_TAGS and FOR_TRACE_TYPES.  Concrete
   examples, probably good enough to use as is. */

#define DECL_TRACE_TAG_MEMBER(name)  const char *name;
#define DECL_TRACE_TAGS_STRUCT() struct trace_tags { FOR_TRACE_TAGS(DECL_TRACE_TAG_MEMBER) }

#define DECL_TRACE_TYPE_MEMBER(name) trace_write_fn name;
#define DECL_TRACE_TYPES_STRUCT() struct trace_types { FOR_TRACE_TYPES(DECL_TRACE_TYPE_MEMBER) }

#define DEF_TRACE_TAG(name) .name = #name,
#define DEF_TRACE_TAGS_STRUCT(name) const struct trace_tags name = { FOR_TRACE_TAGS(DEF_TRACE_TAG) }

#define DEF_TRACE_TYPE(name) .name = trace_write_##name,
#define DEF_TRACE_TYPES_STRUCT(name) const struct trace_types name = { FOR_TRACE_TYPES(DEF_TRACE_TYPE) }

/* Top level definition macro for the C file using standard names. */
#define DEF_TRACE_STRUCTS()\
DEF_TRACE_TAGS_STRUCT(trace_tags); \
DEF_TRACE_TYPES_STRUCT(trace_types); \
const struct trace_base trace_base = { \
    .begin = trace_begin, \
    .end   = trace_end, \
}; \
const struct trace_namespace trace_namespace = { \
    .base  = &trace_base, \
    .tags  = &trace_tags, \
    .types = &trace_types, \
}



#if 0 // OLD, REVISE


/* Binary trace logger.

   The point of this is to be fast, since infof() extended with ascii
   timestamping is very slow.

   So keep it simple:
   0x80 - 0xFF indicates a binary message follows, payload size is 7 LSBs
   4 bytes timestamp, host order
   n bytes payload

*/



#include "cycle_counter.h"

#define TRACE(...) {                                            \
        uint32_t msg[] = {0, cycle_counter(), __VA_ARGS__};     \
        trace_write(((uint8_t*)msg)+3, sizeof(msg)-3);          \
    }
static inline void trace_write(uint8_t *buf, uint32_t size) {
    buf[0] = (size - 4) | 0x80;
    info_write(buf, size);
}
#endif




#endif
