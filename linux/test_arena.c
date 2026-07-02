/* Tests for the arena allocator.

   Some asserts use public properties, some use private (internal
   implementation-dependnet) properties.

*/



#include "mmap_bump.h"
void test1(void) {
    struct arena _a, *a=&_a;
    arena_init(a);

    const char *s0 = "test string";

    char *s1 = arena_strdup(a, s0);
    ASSERT(s1); // pub
    ASSERT(!strcmp(s0,s1)); // pub
    LOG("s1 = '%s' %p\n", s1, s1);

    char *s2 = arena_strdup(a, s0);
    ASSERT(s2); // pub
    ASSERT(!strcmp(s0,s2)); // pub
    ASSERT(s2 > s1); // priv, linear growth
    LOG("s2 = '%s' %p\n", s2, s2);

    arena_swap(a);
    LOG("swap\n");

    char *s3 = arena_strdup(a, s0);
    ASSERT(s3); // pub
    ASSERT(!strcmp(s0,s3)); // pub
    LOG("s1 = '%s' %p\n", s1, s1);
    LOG("s2 = '%s' %p\n", s2, s2);
    LOG("s3 = '%s' %p\n", s3, s3);

    arena_clear(a);
    LOG("clear\n");
    ASSERT(!*s1); // priv, zero after clear
    ASSERT(!*s2); // ..
    ASSERT(*s3); // pub: survivor
    ASSERT(!strcmp(s0,s3)); // pub

    /* Note that dereferencing a cleared pointer is not allowed, but
       it will show up as zeroed. */

    char *s4 = arena_strdup(a, s0);
    ASSERT(s4 > s3); // priv. linear growth
    LOG("s3 = '%s' %p\n", s3, s3);
    LOG("s4 = '%s' %p\n", s4, s4);


    // FIXME: Test the growth

}

#include "mod_sqlite3_arena.c"
#define TEST_REPORT(m)    \
    m(0, s, start_time) \
    m(1, s, hw_config)  \
    m(2, s, prop_name)  \
    m(3, i, prop_index) \
    m(4, i, prop_size)  \
    m(5, i, prop_seed)  \
    m(6, s, status)     \
    m(7, s, log)        \

struct test_report {
    TEST_REPORT(DEF_RECORD_STRUCT)
};
static inline struct test_report *test_report_copy(struct arena *arena, struct test_report *src) {
    struct test_report *dst = NULL;
    TEST_REPORT(DO_RECORD_COPY);
    return dst;
}
static inline struct test_report *test_report_init(struct arena *arena, sqlite3_stmt *query) {
    struct test_report *dst = NULL;
    TEST_REPORT(DO_RECORD_INIT);
    return dst;
}

void test2(void) {
}


int main(void) {
    test1();
    test2();
    return 0;
}
