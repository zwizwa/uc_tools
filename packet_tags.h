#ifndef PACKET_TAGS_H
#define PACKET_TAGS_H

// FIXME: These are not slip-specific, just packet dispatch.

#define TAG_FLASH_ERASE 0xFFF6
#define TAG_FLASH_WRITE 0xFFF7
#define TAG_PLUGCTL     0xFFF8  // loadable plugin control
#define TAG_PLUGIO      0xFFFA  // loadable plugin i/o messages
#define TAG_STREAM      0xFFFB  // byte stream encapsulation.
#define TAG_PING        0xFFFC  // request TAG_REPLY echo
#define TAG_GDB         0xFFFD  // gdbstub RSP tunnel
#define TAG_INFO        0xFFFE  // raw info log stream
#define TAG_REPLY       0xFFFF  // standard reply/ack format

/* TAG_STREAM
   u16_be: byte stream ID
   rest:   byte stream data
*/

#endif
