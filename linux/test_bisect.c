#include "macros.h"
#include <stdint.h>

typedef struct {
    const intptr_t *data;
    intptr_t        n;
} iarr_t;

static inline intptr_t iarr_size(iarr_t *a)             { return a->n; }
static inline intptr_t iarr_rank(iarr_t *a, intptr_t i) { return a->data[i]; }
#define NS(x) iarr##x
#include "ns_bisect.h"
#undef NS


static int failures = 0;
static int checks   = 0;

#define CHECK_EQ(desc, got, want) do {                                  \
    intptr_t g_ = (got), w_ = (want);                                   \
    checks++;                                                           \
    if (g_ != w_) {                                                     \
        failures++;                                                     \
        LOG("FAIL %s:%d %s: got %ld, want %ld\n",                       \
            __FILE__, __LINE__, desc, (long)g_, (long)w_);              \
    }                                                                   \
} while (0)

/* Linear-scan reference implementations */
static intptr_t ref_lower_bound(iarr_t *a, intptr_t rank) {
    for (intptr_t i = 0; i < a->n; i++) {
        if (a->data[i] >= rank) return i;
    }
    return a->n;
}
static intptr_t ref_upper_bound(iarr_t *a, intptr_t rank) {
    for (intptr_t i = 0; i < a->n; i++) {
        if (a->data[i] > rank) return i;
    }
    return a->n;
}
static intptr_t ref_find_first(iarr_t *a, intptr_t rank) {
    for (intptr_t i = 0; i < a->n; i++) {
        if (a->data[i] == rank) return i;
    }
    return -1;
}
static intptr_t ref_find_last(iarr_t *a, intptr_t rank) {
    for (intptr_t i = a->n - 1; i >= 0; i--) {
        if (a->data[i] == rank) return i;
    }
    return -1;
}
static void cross_check(iarr_t *a, intptr_t rank, const char *label) {
    CHECK_EQ(label, iarr_lower_bound(a, rank), ref_lower_bound(a, rank));
    CHECK_EQ(label, iarr_upper_bound(a, rank), ref_upper_bound(a, rank));
    CHECK_EQ(label, iarr_find_first(a, rank),  ref_find_first(a, rank));
    CHECK_EQ(label, iarr_find_last(a, rank),   ref_find_last(a, rank));
    CHECK_EQ(label, iarr_contains(a, rank),    ref_find_first(a, rank) >= 0);
    /* _find may return any duplicate, so only check it agrees on presence
       and, when found, actually points at the rank. */
    intptr_t f = iarr_find(a, rank);
    checks++;
    if (ref_find_first(a, rank) >= 0) {
        if (f < 0 || a->data[f] != rank) {
            failures++;
            LOG("FAIL %s: _find returned %ld for present rank %ld\n",
                label, (long)f, (long)rank);
        }
    } else if (f != -1) {
        failures++;
        LOG("FAIL %s: _find returned %ld for absent rank %ld\n",
            label, (long)f, (long)rank);
    }
}
/* Fixed cases */
static void test_motivating_example(void) {
    /* "1 1 1 2 2 2, the 4th element is where a search for 2 would end up" */
    static const intptr_t d[] = {1, 1, 1, 2, 2, 2};
    iarr_t a = { d, 6 };

    CHECK_EQ("first 2 is index 3",      iarr_find_first(&a, 2),  3);
    CHECK_EQ("first 1 is index 0",      iarr_find_first(&a, 1),  0);
    CHECK_EQ("last 1 is index 2",       iarr_find_last(&a, 1),   2);
    CHECK_EQ("last 2 is index 5",       iarr_find_last(&a, 2),   5);
    CHECK_EQ("lower_bound(2) == 3",     iarr_lower_bound(&a, 2), 3);
    CHECK_EQ("upper_bound(1) == 3",     iarr_upper_bound(&a, 1), 3);
    CHECK_EQ("count of 2s via bounds",
             iarr_upper_bound(&a, 2) - iarr_lower_bound(&a, 2),  3);
}

static void test_empty(void) {
    iarr_t a = { NULL, 0 };
    CHECK_EQ("empty find",        iarr_find(&a, 5),        -1);
    CHECK_EQ("empty find_first",  iarr_find_first(&a, 5),  -1);
    CHECK_EQ("empty find_last",   iarr_find_last(&a, 5),   -1);
    CHECK_EQ("empty lower_bound", iarr_lower_bound(&a, 5),  0);
    CHECK_EQ("empty upper_bound", iarr_upper_bound(&a, 5),  0);
    CHECK_EQ("empty contains",    iarr_contains(&a, 5),     0);
}

static void test_single(void) {
    static const intptr_t d[] = {7};
    iarr_t a = { d, 1 };
    CHECK_EQ("single hit first",     iarr_find_first(&a, 7),  0);
    CHECK_EQ("single hit last",      iarr_find_last(&a, 7),   0);
    CHECK_EQ("single miss below",    iarr_find_first(&a, 6), -1);
    CHECK_EQ("single miss above",    iarr_find_first(&a, 8), -1);
    CHECK_EQ("single lb below",      iarr_lower_bound(&a, 6), 0);
    CHECK_EQ("single lb above",      iarr_lower_bound(&a, 8), 1);
}

static void test_all_same(void) {
    static const intptr_t d[] = {4, 4, 4, 4, 4};
    iarr_t a = { d, 5 };
    CHECK_EQ("all-same first",  iarr_find_first(&a, 4),  0);
    CHECK_EQ("all-same last",   iarr_find_last(&a, 4),   4);
    CHECK_EQ("all-same lb",     iarr_lower_bound(&a, 4), 0);
    CHECK_EQ("all-same ub",     iarr_upper_bound(&a, 4), 5);
    CHECK_EQ("all-same miss",   iarr_find_first(&a, 3), -1);
}

static void test_gaps_and_extremes(void) {
    static const intptr_t d[] = {-10, -10, 0, 3, 3, 3, 9};
    iarr_t a = { d, 7 };
    CHECK_EQ("gap rank absent",       iarr_find_first(&a, 2),  -1);
    CHECK_EQ("gap lb is insertion",   iarr_lower_bound(&a, 2),  3);
    CHECK_EQ("below-min lb",          iarr_lower_bound(&a, -99), 0);
    CHECK_EQ("above-max lb",          iarr_lower_bound(&a, 99),  7);
    CHECK_EQ("above-max find_last",   iarr_find_last(&a, 99),  -1);
    CHECK_EQ("negative first",        iarr_find_first(&a, -10), 0);
    CHECK_EQ("negative last",         iarr_find_last(&a, -10),  1);
    CHECK_EQ("distinct max",          iarr_find_first(&a, 9),   6);
}

/* Randomized cross-check against linear scans */

static int cmp_intptr(const void *pa, const void *pb) {
    intptr_t x = *(const intptr_t *)pa, y = *(const intptr_t *)pb;
    return (x > y) - (x < y);
}

static void test_random(void) {
    enum { ROUNDS = 5000, MAX_N = 100 };
    intptr_t buf[MAX_N];
    //srand(12345); /* deterministic */

    for (int r = 0; r < ROUNDS; r++) {
        intptr_t n = rand() % (MAX_N + 1);
        // LOG("%d,%d\r", r, n);
        /* Small value range so duplicates are common. */
        for (intptr_t i = 0; i < n; i++)
            buf[i] = rand() % 10;
        qsort(buf, (size_t)n, sizeof buf[0], cmp_intptr);

        //for (int i=0; i<n; i++) { LOG(" %d", buf[i]); } LOG("\n");

        iarr_t a = { buf, n };

        /* Probe every value in and just outside the range. */
        for (intptr_t rank = -1; rank <= 10; rank++) {
            cross_check(&a, rank, "random");
        }
    }
}

int main(void) {
    test_motivating_example();
    test_empty();
    test_single();
    test_all_same();
    test_gaps_and_extremes();
    test_random();

    if (failures) {
        LOG("%d/%d checks FAILED\n", failures, checks);
        return 1;
    }
    LOG("all %d checks passed\n", checks);
    return 0;
}

