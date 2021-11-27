#ifndef TRACE_H
#define TRACE_H

/* Structured trace logging, API.

   We assume all tags are known globally and statically, meaning the
   structure type itself is defined to contain all tags and formats.
   For most embedded software this is sufficient.  It also makes it
   possible to let the compiler check that all tags are listed,
   e.g. so we know what tags a certain image can produce. */

/* API & MACRO IMPL */
struct trace_namespace;
const struct trace_namespace *trace_namespace();

/* Create local 'ns' scope and execute pre/post functions by abusing a
   for(;;) construct and C expression syntax. */
#define TRACE(ns,tag,...)                                               \
    for(const struct trace_namespace *ns =                              \
            ({const struct trace_namespace *ns0 = trace_namespace();    \
                ns0->begin(ns0->tags->tag);                             \
                ns0;});                                                 \
        ns;                                                             \
        ns=({ns->end();(typeof(ns))0;}))



#if 0 // OLD, REVISE


/* Binary trace logger.

   The point of this is to be fast, since infof() extended with ascii
   timestamping is very slow.

   So keep it simple:
   0x80 - 0xFF indicates a binary message follows, payload size is 7 LSBs
   4 bytes timestamp, host order
   n bytes payload

*/



#include "cycle_counter.h"

#define TRACE(...) {                                            \
        uint32_t msg[] = {0, cycle_counter(), __VA_ARGS__};     \
        trace_write(((uint8_t*)msg)+3, sizeof(msg)-3);          \
    }
static inline void trace_write(uint8_t *buf, uint32_t size) {
    buf[0] = (size - 4) | 0x80;
    info_write(buf, size);
}
#endif




#endif
