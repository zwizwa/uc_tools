#ifndef TRACE_IMPL_TEXT_H
#define TRACE_IMPL_TEXT_H

/* Macros for declaring structures and defining structure instances
   based on list macros FOR_TRACE_TAGS and FOR_TRACE_TYPES.

   If performance doesn't matter, this is probably a good default.
   Log tags are strings, and the types are functions
   trace_text_<type>.
*/
struct trace_base {
    void (*begin)(const char *);
    void (*end)(void);
};


#define DECL_TRACE_TAG_MEMBER(name)  const char *name;
#define DECL_TRACE_TAGS_STRUCT() struct trace_tags { FOR_TRACE_TAGS(DECL_TRACE_TAG_MEMBER) }

#define DECL_TRACE_TYPE_MEMBER(name) trace_write_fn name;
#define DECL_TRACE_TYPES_STRUCT() struct trace_types { FOR_TRACE_TYPES(DECL_TRACE_TYPE_MEMBER) }

#define DEF_TRACE_TAG(name) .name = #name,
#define DEF_TRACE_TAGS_STRUCT(name) const struct trace_tags name = { FOR_TRACE_TAGS(DEF_TRACE_TAG) }

#define DEF_TRACE_TYPE(name) .name = trace_text_##name,
#define DEF_TRACE_TYPES_STRUCT(name) const struct trace_types name = { FOR_TRACE_TYPES(DEF_TRACE_TYPE) }

/* Top level definition macro for the C file using standard names. */
#define DEF_TRACE_STRUCTS()\
DEF_TRACE_TAGS_STRUCT(trace_tags); \
DEF_TRACE_TYPES_STRUCT(trace_types); \
const struct trace_base trace_base = { \
    .begin = trace_text_begin, \
    .end   = trace_text_end, \
}; \
const struct trace_namespace trace_namespace = { \
    .base  = &trace_base, \
    .tags  = &trace_tags, \
    .types = &trace_types, \
}

#endif
