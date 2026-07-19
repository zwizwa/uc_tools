/* Add doubly-linked-list methods to a datastructure with prev/next pointers.
   Pointer and payloads are all  contained in the node structure.
   Simplified from https://claude.ai/chat/3085ba73-3ebf-4526-8171-560ab5e67fdd
*/


/* Initialize the sentinel. */
static inline void NS(_init)(NS(_t) *l) {
    memset(l,0,sizeof(*l));
    l->next = l->prev = l;
}

/* Push an element to the head and tail of the list. */
static inline void NS(_push_head)(NS(_t) *l, NS(_t) *e) {
    ASSERT(!e->next); // PRE: element is not in any list
    e->next = l->next;
    e->prev = l;
    l->next->prev = e;
    l->next = e;
}
static inline void NS(_push_tail)(NS(_t) *l, NS(_t) *e) {
    ASSERT(!e->next); // PRE: element is not in any list
    e->next = l;
    e->prev = l->prev;
    l->prev->next = e;
    l->prev = e;
}

static inline void NS(_remove)(NS(_t) *e) {
    ASSERT(e->next); // PRE: element is in a list
    e->next->prev = e->prev;
    e->prev->next = e->next;
    e->next = 0; // POST: element is not in any list
    e->prev = 0;
}

static inline NS(_t) *NS(_pop_head)(NS(_t) *l) {
    NS(_t) *e = l->next;
    if (e == l) return 0;
    NS(_remove)(e);
    return e;
}

static inline NS(_t) *NS(_pop_tail)(NS(_t) *l) {
    NS(_t) *e = l->prev;
    if (e == l) return 0;
    NS(_remove)(e);
    return e;
}

