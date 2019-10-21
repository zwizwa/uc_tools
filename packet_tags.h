#ifndef PACKET_TAGS_H
#define PACKET_TAGS_H

// FIXME: These are not slip-specific, just packet dispatch.

#define TAG_PLUGIN 0xFFFA  // loadable plugin i/o port
#define TAG_UART   0xFFFB  // serial port bridge

#define TAG_PING   0xFFFC  // request TAG_REPLY echo
#define TAG_GDB    0xFFFD  // gdbstub RSP tunnel
#define TAG_INFO   0xFFFE  // info log data
#define TAG_REPLY  0xFFFF  // standard reply/ack format

#endif
