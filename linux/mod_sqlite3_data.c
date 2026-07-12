#ifndef MOD_SQLITE3_DATA
#define MOD_SQLITE3_DATA

/* Basic sqlite query interface. */
#include "mod_sqlite3.c"

/* The rest is data representation and relies on the bump allocator. */
#include "mmap_bump.h"

/* Basic types that can appear in SQL tables. */
typedef const char *text;
typedef int         integer;

/* The functions that depend on the allocator are defined elsewhere.
   We assume it is enough to just leave the type abstract and assume
   that the constructors can be defined globally. */
static inline text copy_text(struct arena *a, text x) {
    return arena_strdup(a, x);
}
static inline integer copy_integer(struct arena *a, integer x) {
    return x;
}

/* Map the sqlite accessors to ct_* types.  These do not do any
   allocation but are only valid between cursor updates. */
static inline text from_query_text(
    struct arena *a, struct sqlite3_stmt *q, int index)
{
    text field = (text)sqlite3_column_text(q, index);
    if (field) { field = arena_strdup(a, field); }
    return field;
}
static inline integer from_query_integer(
    struct arena *a, struct sqlite3_stmt *q, int index)
{
    return sqlite3_column_int(q, index);
}


/* These assume lexical context:
   sqlite3_stmt *query         current query
   struct arena *arena         the allocator
   struct <record> *dst, *src  the C struct representing source or destination record
*/

/* Define a struct field. */
#define DEF_RECORD_STRUCT(index, type, name) \
    type name;

/* A member from_query from sqlite cursor context */
#define DO_RECORD_FROM_QUERY(index, type, name) \
    dst->name = from_query_##type(arena, query, index);

/* A member copy */
#define DO_RECORD_COPY(index, type, name) \
    dst->name = copy_##type(arena, src->name);



#define DEF_TABLE(name, macro) \
/* C struct and object = pointer to struct instance. */ \
/* All structs have a prev pointer to allow linking during query traversal. */ \
struct name; \
struct name { \
    macro(DEF_RECORD_STRUCT) \
    struct name *prev; \
}; \
typedef struct name *name; \
/* Copy constructor */ \
static inline name name##_copy(struct arena *arena, name src) { \
    name dst = arena_alloc(arena, sizeof(*dst)); \
    macro(DO_RECORD_COPY); \
    return dst; \
} \
/* From sqlite cursor constructor */ \
static inline name name##_from_query(struct arena *arena, sqlite3_stmt *query) { \
    name dst = arena_alloc(arena, sizeof(*dst)); \
    macro(DO_RECORD_FROM_QUERY); \
    return dst; \
} \




#endif
