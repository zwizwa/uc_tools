/* NS module for min/max mipmap.

   This is a module because it is parameterized "at the bottom" by the
   base sample type. */

#include "mmap_file.h"

#ifndef MINMAX_LOG
#define MINMAX_LOG(...)
#endif

struct NS(_minmax) { NS(_t) min, max; };
static inline NS(_t) NS(_max)(NS(_t) a, NS(_t) b) { return a > b ? a : b; }
static inline NS(_t) NS(_min)(NS(_t) a, NS(_t) b) { return a < b ? a : b; }

/* Computation of min/max multires tree.

   For now, keep it simple.  I'm not yet sure how to manage the
   resources, so:

   - Don't do incremental yet.

   - Stick to int16_t as data type.
*/

#define LEVEL_ENDX 20
struct NS(_map) {
    uintptr_t level_start;
    /* Original file is mapped at 0.
       The min/max levels are mapped starting at 1.
       This keeps the nb_at_level invariant. */
    struct mmap_file level[LEVEL_ENDX];
};

static inline NS(_t)* NS(_original)(struct NS(_map) *s) {
    return s->level[0].buf;
}
static inline struct NS(_minmax)* NS(_level)(struct NS(_map) *s, uintptr_t level) {
    ASSERT(level > 0);
    ASSERT(level < LEVEL_ENDX);
    return s->level[level].buf;
}
/* Always get the array bounds right to make refactoring simpler.
   This is used for allocation and for array dereference. */
static inline off_t NS(_nb_at_level)(struct NS(_map) *s, uintptr_t level) {
    off_t nb_in = s->level[0].size / sizeof(NS(_t));
    return nb_in >> level;
}

/* Compute directly */
static inline struct NS(_minmax) NS(_compute)(NS(_t) *orig, off_t abs, uintptr_t level) {

    /* Compute the original coordinates from abs.  Because abs is
       within bounds and each level>0 sample has full parent extent,
       the original coordinates are as well. */
    NS(_t) *o      = &orig[abs     << level];
    NS(_t) *o_endx = &orig[(abs+1) << level];
    struct NS(_minmax) mm = {
        .min = *o,
        .max = *o,
    };
    while(++o < o_endx) {
        mm.min = NS(_min)(mm.min, *o);
        mm.max = NS(_max)(mm.max, *o);
    }
    return mm;
}
static inline intptr_t NS(_open)(struct NS(_map) *s, const char *file,
                                 uintptr_t level_start) {
    ASSERT(level_start > 1);
    ASSERT(level_start < LEVEL_ENDX);
    memset(s,0,sizeof(*s));
    s->level_start = level_start;
    mmap_file_open_ro(&s->level[0], file);

    char tmp[1024];
    snprintf(tmp, sizeof(tmp), "mkdir -p '%s.d'\n", file);
    system(tmp);

    for (int level=s->level_start; level<LEVEL_ENDX; level++) {

        snprintf(tmp, sizeof(tmp), "%s.d/%02d.minmax", file, level);

        off_t nb_at_level = NS(_nb_at_level)(s, level);
        off_t bytes_at_level = nb_at_level * sizeof(struct NS(_minmax));
        mmap_file_open(&s->level[level], tmp, bytes_at_level);
        struct NS(_minmax) *mm = NS(_level)(s, level);

        if (level == s->level_start) {
            NS(_t) *orig = NS(_original)(s);

            /* The first level needs to be computed from the original. */
            for (off_t i=0; i<nb_at_level; i++) {
                mm[i] = NS(_compute)(orig, i, level);
            }
        }
        else {
            /* Other levels are computed recursively. */
            struct NS(_minmax) *mm1 = NS(_level)(s, level-1);
            for (off_t i=0; i<nb_at_level; i++) {
                mm[i].min = NS(_min)(mm1[2*i].min, mm1[2*i+1].min);
                mm[i].max = NS(_max)(mm1[2*i].max, mm1[2*i+1].max);
            }
        }
    }
    return 0;
}



/* Keep track of navigation through the file.
   We will interpret up/down at window offset. */
struct NS(_cursor) {
    off_t point;    /* Last point into file, at coarse scale res. */
    int16_t win_x;  /* Last x coordinate, corresponds to point. */
    int16_t level;  /* Current level. */
} cursor;
void NS(_cursor_init)(struct NS(_cursor) *c) {
    ZERO(c);
}



/* Return value is actual level: we decide whether it makes sense to
   zoom out further.  The buffer is always valid.  Off-edge values are
   filled with min/max at zero. */
int16_t NS(_cursor_zoom)(
    struct NS(_cursor) *c,
    struct NS(_map) *s,
    struct NS(_minmax) *buf, // Holds win_w elements
    int16_t win_w,           // Window width
    int16_t win_h,           // Window height
    int16_t win_x,           // Mouse pointer offset
    int16_t level_inc) {     // Increment +1,-1

    /* Find the new point by converting pixels to point offset, which
       is stored as a level 0 offset. */
    {
        c->point += (win_x - c->win_x) * (1 << c->level);
        c->win_x = win_x;
    }

    /* Determine what level to render at, clipping low and high. */
    {
        int16_t next_level = c->level + level_inc;
        if (next_level < 0) next_level = 0;
        if (next_level >= LEVEL_ENDX) next_level = LEVEL_ENDX-1;
        c->level = next_level;
    }

    /* Find the window into the desired level minmax array. */
    {
        /* Current level coordinate of left window edge. */
        off_t left_abs  = (c->point >> c->level) - win_x;

        /* Number of samples at this level, for bounds checking. */
        off_t nb = NS(_nb_at_level)(s, c->level);

        /* Fill in the window in window coordinates, with out-of-bound
           represented as midline. */
        intptr_t of = win_h / 2;
        intptr_t sc = 0xFFFF / win_h;

        struct NS(_minmax) zero = { .min = of, .max = of};
        if (c->level < s->level_start) {
            MINMAX_LOG("compute %d\n", c->level);
            /* For the finest level we do not keep minmax data, as the
               storage requirements are large, and it can just as well
               be recomputed. */
            NS(_t) *orig = NS(_original)(s);
            for(off_t rel=0; rel<win_w; rel++) {
                off_t abs = left_abs + rel;
                if ((abs >= 0) && (abs < nb)) {
                    struct NS(_minmax) mm = NS(_compute)(orig, abs, c->level);
                    buf[rel].min = of - mm.min / sc;
                    buf[rel].max = of - mm.max / sc;
                }
                else {
                    buf[rel] = zero;
                }
            }
        }
        else {
            MINMAX_LOG("cached %d\n", c->level);
            struct NS(_minmax) *mm = NS(_level)(s, c->level);
            for(off_t rel=0; rel<win_w; rel++) {
                off_t abs = left_abs + rel;
                if ((abs >= 0) && (abs < nb)) {
                    buf[rel].min = of - mm[abs].min / sc;
                    buf[rel].max = of - mm[abs].max / sc;
                }
                else {
                    buf[rel] = zero;
                }
            }
        }
        MINMAX_LOG("left_abs = %d\n", left_abs);
    }

    return c->level;
}

