#ifndef DLIST_H
#define DLIST_H

/* Doubly linked list.

   Inspired by linux/list.h, but we use simplified semantics.  Our
   memory model is linear, so we only ever move elements from one list
   to another.  There is no concept of an isolated element, only that
   of a singleton list.  This removes the need for "add" and "del"
   operations.
*/

struct dlist;
struct dlist {
    struct dlist *next;
    struct dlist *prev;
};
/* The base state is a singleton. */
#define DLIST_INIT(d) {&(d),&(d)}
static inline void dlist_init(struct dlist *entry) {
    entry->prev = entry;
    entry->next = entry;
}
static inline int dlist_singleton(struct dlist *entry) {
    if (entry->next == entry) {
        ASSERT(entry->prev == entry);
        return 1;
    }
    else {
        return 0;
    }
}

/* The only mutating operation besides initialization as singeton is
   to move a singleton from one list to another. */
static inline void __dlist_del(struct dlist * prev, struct dlist * next) {
    next->prev = prev;
    prev->next = next;
}
static inline void __dlist_del_entry(struct dlist *entry) {
    __dlist_del(entry->prev, entry->next);
}
static inline void __dlist_add(struct dlist *new,
                               struct dlist *prev,
                               struct dlist *next) {
    next->prev = new;
    new->next = next;
    new->prev = prev;
    prev->next = new;
}
static inline void dlist_move(struct dlist *element, struct dlist *head) {
    __dlist_del_entry(element);
    __dlist_add(element, head, head->next);
}



#endif
