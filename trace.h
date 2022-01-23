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

   For implementation see:
   trace_text.h
   trace_bin.h

   Those files provide a number of macros to declare and define the
   structure types that create the lists of tags and types.

 */

#include <stdint.h>

/* API  */
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

struct trace_base;
struct trace_tags;
struct trace_types;
struct trace_namespace {
    const struct trace_tags  *tags;
    const struct trace_types *types;
    const struct trace_base  *base;
};

typedef void (*trace_write_fn)(uintptr_t arg);

#endif
