/* Stripped down C version of constrained parametric polymorphism
   (akin to type classes, ML functors, mixins).

   based on:
   - an NS prefixing macro to reference types and define names
   - reliance on field names in the C types (interface)

   references:
   NS(_container_t), NS(_element_t)

*/
#ifndef NS
#error define NS
#endif

#include <stdint.h>

/* O(1) */

// read/write slots can be NULL to indicate empty/full conditions.
static inline const NS(_element_t) *NS(_peek)(NS(_container_t) *q) {
    if (q->read == q->write) return NULL;
    else return &q->buf[q->read];
}
static inline const NS(_element_t) *NS(_read_hole)(NS(_container_t) *q) {
    if (q->read == q->write) return NULL;
    NS(_element_t) *slot = &q->buf[q->read];
    q->read = (q->read + 1) % ARRAY_SIZE(q->buf);
    return slot;
}
static inline uint32_t NS(_nb_stored)(NS(_container_t) *q) {
    const uint32_t s = ARRAY_SIZE(q->buf);
    return (s + q->write - q->read) % s;
}
static inline uint32_t NS(_nb_available)(NS(_container_t) *q) {
    return ARRAY_SIZE(q->buf) - 1 - NS(_nb_stored)(q);
}
static inline NS(_element_t) *NS(_write_hole)(NS(_container_t) *q) {
    if (NS(_nb_available)(q) == 0) return NULL;
    NS(_element_t) *slot = &q->buf[q->write];
    q->write = (q->write + 1) % ARRAY_SIZE(q->buf);
    return slot;
}
static inline void NS(_read)(NS(_container_t) *q, NS(_element_t) *e) {
    const NS(_element_t) *slot = NS(_read_hole)(q);
    if (slot) *e = *slot;
}
static inline void NS(_drop)(NS(_container_t) *q) {
    NS(_read_hole)(q);
}
// Note that the return pointer is only valid until the next shift around.
static inline NS(_element_t) *NS(_write)(NS(_container_t) *q, const NS(_element_t) *e) {
    NS(_element_t) *slot = NS(_write_hole)(q);
    if (slot) *slot = *e;
    return slot;
}
static inline NS(_element_t) *NS(_shift)(NS(_container_t) *q) {
    const NS(_element_t) *slot = NS(_read_hole)(q);
    if (!slot) return NULL;
    return NS(_write)(q, slot);
}
static inline void NS(_clear)(NS(_container_t) *q) {
    q->write = 0;
    q->read = 0;
}
static inline void NS(_cycle)(NS(_container_t) *q) {
    NS(_element_t) e = {};
    NS(_read)(q, &e);
    NS(_write)(q, &e);
}



