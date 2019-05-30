/* Iterators to supplement ns_queue.h

   Needs in addition:
   NS(_predicate_t), NS(_predicate_context_t)

*/


/* Voice allocator "stack with random remove".

   A voice allocator is basiclly a stack with random removal and
   N-peek at the head:

   - Add to the head (= note on)
   - Remove from within the body (= note off)
   - Iterate over or othewise reify the first N slots (= voice allocation)

   The allocation step will be executed on each audio block, so that's
   the one that needs to be convenient.

   The others do not happen frequently, but should not cause large
   spikes either because they probably run in a real-time context.
   It's probably ok to have linear complexity as long as the data
   structure is cache-friendly.

   Additionally, since we do not control the input event stream and
   could always send more events than the size of the structure, there
   needs to be a strategy to drop "on" events.  The most
   straightforward way is to use a queue to implement the stack, and
   just drop head or tail on queue full, hence why this comment is in
   this file.

   So practically, the queue data structure needs to be extended with
   a removal step.  Note that the removal is not atomic.

*/

#ifndef NS
#error define NS
#endif

typedef int (*NS(_predicate_t))(void*, const NS(_element_t) *);


/* Remove elements based on a predicate.

   Implement it as an O(N) cycle.  Since this is intended for
   real-time use, only the peak load matters, so don't bother with
   moving only part of the queue.

   Note that it is probably possible to generailze the iteration frame
   for any operation that rebuilds the queue based on cycling through
   the elements. */

static inline int NS(_remove)(
    NS(_container_t) *q,
    NS(_predicate_t) pred,
    void *ctx) {

    int removed = 0;

    uint32_t endx = q->write;
    for(;;) {
        const NS(_element_t) *e = NS(_peek(q));
        // don't cycle an empty queue
        if (!e) break;
        // don't go through the queue more than once
        if (q->read == endx) break;
        if (pred(ctx, e)) {
            NS(_drop)(q);
            removed++;
        }
        else {
            NS(_cycle)(q);
        }
    }
    return removed;
}

/* Read-only iteration.  Return value of the predicate is used to
 * continue. */

static inline void NS(_fold)(
    NS(_container_t) *q,
    NS(_predicate_t) pred,
    void *ctx) {

    uint32_t i    = q->read;
    uint32_t endx = q->write;

    while (i != endx) {
        const NS(_element_t) *e = &q->buf[i];
        if (!pred(ctx, e)) break;
        i  = (i + 1) % ARRAY_SIZE(q->buf);
    }
}



/* This is considered to be a local parameter to this file, so drop it
 * out of scope. */
#undef NS
