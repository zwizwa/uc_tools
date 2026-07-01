// FIXME: Add this to pbuf interface after generalization to ns_pbuf

#ifndef ARRAY_GROW_H
#define ARRAY_GROW_H

static inline void array_grow(void **array,
                              uintptr_t el_size,
                              uintptr_t *count,
                              uintptr_t *room) {
    if (!(*array)) {
        /* Set a reasonable initial size. */
        *room = 100;
        *count = 0;
        *array = malloc(el_size * (*room));
    }
    if (*count >= *room) {
        *room *= 2;
        *array = realloc(*array, el_size * (*room));
    }
    (*count)++;
}
#define ARRAY_GROW(array) \
    array_grow((void **)&((array).buf), sizeof(((array).buf)[0]), &((array).count), &((array).room))

#endif
