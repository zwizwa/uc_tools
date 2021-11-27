#ifndef TRACE_H
#define TRACE_H

/* Structured trace logging, API.

   We assume all tags are known globally and statically, meaning the
   structure type itself is defined to contain all tags and formats.
   For most embedded software this is sufficient.  It also makes it
   possible to let the compiler check that all tags are listed,
   e.g. so we know what tags a certain image can produce. */

#include <stdint.h>

/* API & MACRO IMPL */
struct trace_namespace;
extern const struct trace_namespace trace_namespace;

/* Create local 'ns' scope and execute pre/post functions by abusing a
   for(;;) construct and C expression syntax. */
#define TRACE_NS(ns,t,tag,...)                                          \
    for(const struct trace_types *t =                                   \
            ({ns->base->begin(ns->tags->tag); ns->types;});             \
        t;                                                              \
        t=({ns->base->end();(typeof(t))0;}))

#define TRACE(t,tag,...) \
    TRACE_NS((&trace_namespace),t,tag,__VA_ARGS__)

struct trace_base {
    void (*begin)(const char *dmx);
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
