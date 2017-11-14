/* To the extent possible under law, Tom Schouten has waived all
   copyright and related or neighboring rights to sink.h and sink.c
   Code:    http://zwizwa.be/git/uc_tools
   License: http://creativecommons.org/publicdomain/zero/1.0 */

#ifndef SINK_H
#define SINK_H

#include <stdint.h>

// TODO: unifiy with sm.h

// Abstraction used to replace buffer write with abstract write.
struct sink;
typedef void (*sink_write_fn)(struct sink *, uint8_t byte);
typedef int  (*sink_ready_fn)(struct sink *);

struct sink {
    sink_ready_fn ready;
    sink_write_fn write;
};


// For concrete buffer implementation
struct sink_buffer {
    struct sink api;
    uint8_t *buf;
    uint32_t write;  // &buf[write] is hole for next write
    uint32_t size;   // buffer size
};
void sink_buffer_init(struct sink_buffer *sink_buffer, uint8_t *buf, uint32_t size);
uint32_t sink_write(struct sink *s, const uint8_t *src, uint32_t nb);


#endif //SINK_H
