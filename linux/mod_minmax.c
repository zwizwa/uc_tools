#ifndef MOD_MINMAX
#define MOD_MINMAX

#include "mmap_file.h"
#include "macros.h"

/* Computation of min/max multires tree.

   For now, keep it simple.  I'm not yet sure how to manage the
   resources, so:

   - Don't do incremental yet.

   - Stick to int16_t as data type.
*/

#define NB_LEVELS 20
struct mipmap {
    off_t nb_at_0;
    struct mmap_file level[NB_LEVELS];
};

struct mipmap mipmap;

/* Basic data type. */
typedef int16_t t;
struct minmax { t min, max; };
static inline t max(t a, t b) { return a > b ? a : b; }
static inline t min(t a, t b) { return a < b ? a : b; }

/* Always get the array bounds right to make refactoring simpler.
   This is used for allocation and for array dereference. */
static inline off_t mipmap_nb_at_level(struct mipmap *s, uintptr_t level) {
    return s->nb_at_0 >> level;
}

intptr_t generate_minmax(const char *file, const char *dir) {
    LOG("test_mipmap\n");
    struct mmap_file f_in; mmap_file_open_ro(&f_in, file);
    off_t nb_in = f_in.size / sizeof(t);
    t *in = f_in.buf;

    /* Needs to be initialized before mipmap_nb_at_level works. */
    mipmap.nb_at_0 = nb_in / 2;


    char tmp[1024];
    snprintf(tmp, sizeof(tmp), "mkdir -p '%s'\n", dir);
    system(tmp);

    for (int level=0; level<NB_LEVELS; level++) {

        snprintf(tmp, sizeof(tmp), "%s/%03d", dir, level);

        off_t nb_at_level = mipmap_nb_at_level(&mipmap, level);
        off_t bytes_at_level = nb_at_level * sizeof(struct minmax);
        struct minmax *mm = mmap_file_open(
            &mipmap.level[level], tmp, bytes_at_level);

        if (level == 0) {
            /* All the rest can be dervied from this. */

            /* First level is initialized from file. */
            for (off_t i=0; i<nb_at_level; i++) {
                mm[i].min = min(in[2*i], in[2*i+1]);
                mm[i].max = max(in[2*i], in[2*i+1]);
            }
        }
        else {
            /* Other levels use the previous mipmap. */
            struct minmax *mm1 = mipmap.level[level-1].buf;
            for (off_t i=0; i<nb_at_level; i++) {
                mm[i].min = min(mm1[2*i].min, mm1[2*i+1].min);
                mm[i].max = max(mm1[2*i].max, mm1[2*i+1].max);
            }
        }
    }
    return 0;
}

/* Keep track of navigation through the file.
   We will interpret up/down at window offset. */
struct cursor {
    off_t point;    /* Last point into file, at coarse scale res. */
    int16_t win_x;  /* Last x coordinate, corresponds to point. */
    int16_t level;  /* Current level. */
} cursor;
void cursor_init(struct cursor *c) {
    ZERO(c);
}

/* Return value is actual level: we decide whether it makes sense to
   zoom out further.  The buffer is always valid.  Off-edge values are
   filled with min/max at zero. */
int16_t snapshot_window(struct minmax *buf,   // Holds win_w elements
                        int16_t win_w,        // Window width
                        int16_t win_h,        // Window height
                        int16_t win_x,        // Mouse pointer offset
                        int16_t level_inc) {  // Increment +1,-1

    /* Find the new point by converting pixels to point offset, which
       is stored as a level 0 offset. */
    {
        cursor.point += (win_x - cursor.win_x) * (1 << cursor.level);
        cursor.win_x = win_x;
    }

    /* Determine what level to render at, clipping low and high. */
    {
        int16_t next_level = cursor.level + level_inc;
        if (next_level < 0) next_level = 0;
        if (next_level >= NB_LEVELS) next_level = NB_LEVELS-1;
        cursor.level = next_level;
    }

    /* Find the window into the desired level minmax array. */
    {
        /* Current level coordinate of left window edge. */
        off_t left_abs  = (cursor.point >> cursor.level) - win_x;

        /* Number of samples at this level, for bounds checking. */
        off_t nb = mipmap_nb_at_level(&mipmap, cursor.level);

        /* Fill in the window in window coordinates, with out-of-bound
           represented as midline. */
        intptr_t o = win_h / 2;
        intptr_t s = 0xFFFF / win_h;

        struct minmax zero = { .min = o, .max = o};
        struct minmax *mm = mipmap.level[cursor.level].buf;
        for(off_t rel=0; rel<win_w; rel++) {
            off_t abs = left_abs + rel;
            if ((abs >= 0) && (abs < nb)) {
                buf[rel].min = o - mm[abs].min / s;
                buf[rel].max = o - mm[abs].max / s;
            }
            else {
                buf[rel] = zero;
            }
        }
        LOG("left_abs = %d\n", left_abs);
    }

    return cursor.level;
}



#endif
