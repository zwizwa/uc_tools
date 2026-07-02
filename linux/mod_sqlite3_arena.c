#ifndef MOD_SQLITE3_ARENA
#define MOD_SQLITE3_ARENA

/* Basic sqlite query interface. */
#include "mod_sqlite3.c"

/* The rest assumes we are using the arena bump allocator to represent
   query results. */
#include "mmap_bump.h"



/* Simple macro-based column + type wrapping.
   Here "ct" is short for "column type" */
typedef const char *ct_s;
typedef int         ct_i;

/* The functions that depend on the allocator are defined elsewhere.
   We assume it is enough to just leave the type abstract and assume
   that the constructors can be defined globally. */
static inline ct_s copy_ct_s(struct arena *a, ct_s x) {
    return arena_strdup(a, x);
}
static inline ct_i copy_ct_i(struct arena *a, ct_i x) {
    return x;
}

/* Map the sqlite accessors to ct_* types.  These do not do any
   allocation but are only valid between cursor updates. */
static inline ct_s init_ct_s(struct arena *a, struct sqlite3_stmt *q, int index) {
    return arena_strdup(a, (ct_s)sqlite3_column_text(q, index));
}
static inline ct_i init_ct_i(struct arena *a, struct sqlite3_stmt *q, int index) {
    return sqlite3_column_int(q, index);
}


/* These assume lexical context:
   sqlite3_stmt *query         current query
   struct arena *arena         the allocator
   struct <record> *dst, *src  the C struct representing source or destination record
*/

/* Define a struct field. */
#define DEF_RECORD_STRUCT(index, type, name) \
    ct_##type name;

/* A member init from sqlite cursor context */
#define DO_RECORD_INIT(index, type, name) \
    dst->name = init_ct_##type(arena, query, index);

/* A member copy */
#define DO_RECORD_COPY(index, type, name) \
    dst->name = copy_ct_##type(arena, src->name);



#include "array_grow.h"

struct table;
typedef void (*table_format_fn)(struct table *, uintptr_t index, char *buf, uintptr_t buf_size);
typedef void (*table_enter_fn)(struct table *, uintptr_t index);
typedef uintptr_t (*table_size_fn)(struct table *);

struct table {
    table_format_fn format;
    table_enter_fn enter;
    table_size_fn size;
};


#define DEF_TABLE(name) \
struct name##_table { \
    struct table table; \
    struct name *buf; \
    uintptr_t count; \
    uintptr_t room; \
}; \
static inline struct name *name##_grow(struct name##_table *t) { \
    ARRAY_GROW(*t); \
    ASSERT(t->buf); \
    return &t->buf[t->count-1]; \
}



#endif
