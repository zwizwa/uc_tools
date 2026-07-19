/* List for LRU data structure.
   This is basically ns_dlist.h with added backpointer to sentinel. */
typedef NS(_lru_t) NS(_dlist_t);
#include "ns_dlist.h"

static inline void NS(_lru_init)(NS(_lru_t) *l) {
    NS(_dlist_init)(l);
    l->sentinel = l;
}
static inline void NS(_lru_push_head)(NS(_lru_t) *l, NS(_lru_t) *e) {
    NS(_dlist_push_head)(l, e);
    e->sentinel = l;
}
static inline void NS(_lru_push_tail)(NS(_lru_t) *l, NS(_lru_t) *e) {
    NS(_dlist_push_tail)(l, e);
    e->sentinel = l;
}
static inline void NS(_lru_remove)(NS(_lru_t) *e) {
    NS(_dlist_remove)(e);
    e->sentinel = NULL;
}
static inline NS(_lru_t) *NS(_lru_pop_head)(NS(_lru_t) *l) {
    return NS(_dlist_pop_head)(l);
}
static inline NS(_lru_t) *NS(_lru_pop_tail)(NS(_lru_t) *l) {
    return NS(_dlist_pop_tail)(l);
}

/* The additional operation is then mark as recently used.
   Together with pop_tail this is enough to implement an LRU cache. */
static inline void NS(_lru_use)(NS(_lru_t) *e) {
    NS(_dlist_remove(e));
    NS(_dlist_push_head(e->sentinel, e));
}
