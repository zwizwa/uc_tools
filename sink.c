/* To the extent possible under law, Tom Schouten has waived all
   copyright and related or neighboring rights to sink.h and sink.c
   Code:    http://zwizwa.be/git/uc_tools
   License: http://creativecommons.org/publicdomain/zero/1.0 */

#include "sink.h"

// Concrete buffer implementation abstracted behind struct sink.
static int sink_buffer_ready(struct sink_buffer *s) {
    return s->write < s->size;
}
static void sink_buffer_write(struct sink_buffer *s, uint8_t byte) {
    // Can't handle error here.  Dropping data is likely better than
    // writing past the end.
    if (s->write >= s->size) return;
    s->buf[s->write++] = byte;
}
void sink_buffer_init(struct sink_buffer *s, uint8_t *buf, uint32_t size) {
    s->buf = buf;
    s->write = 0;
    s->size = size;
    s->api.ready = (sink_ready_fn)sink_buffer_ready;
    s->api.write = (sink_write_fn)sink_buffer_write;
}
uint32_t sink_write(struct sink *s, const uint8_t *src, uint32_t nb) {
    uint32_t i = 0;
    for(uint32_t i=0; i < nb && s->ready(s); i++) {
        s->write(s, src[i]);
    }
    return i;
}
