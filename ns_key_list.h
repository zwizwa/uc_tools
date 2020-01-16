
#include "ns_list.h"

/* In addition, this requires NS(_key_t) and .key member. */

/* Remove returns a cell, but can match on key. */
static inline NS(_list_t) *NS(_remove)(NS(_list_t) **l, NS(_key_t) k) {
    while(*l) {
        if ((*l)->key == k) { return NS(_pop)(l); }
        l = &((*l)->next);
    }
    return NULL;
}





