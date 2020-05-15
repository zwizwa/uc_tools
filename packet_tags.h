#ifndef PACKET_TAGS_H
#define PACKET_TAGS_H

/* The uc_tools procotol principles:

   - The packet protocol uses a 16 bit little endian tag to multiplex
     packet tyes.  The 0xFF__ range is reserved for the uc_tools
     library.  The rest of the tag space is for application-specific
     packat types.

   - If an application receives an unknown packet type, it should be
     ignored, and possibly logged using an application-specific
     mechanism.

   - To encode packet streams in byte streams, the packet_bridge.c
     utility is provided.  This supports e.g. SLIP and size prefix
     framing methods.

   - uc_tools assumes error-free and in some cases in-order transport.
     For transport media that are not error free, the encapsulation
     layer should implement some kind of error detection/correction
     mechanism.

   - The uc_tools protocols are intended to remain backwards
     compatible

*/

#define TAG_U32         0xFFF5  // skeleton for flat u32 commands
#define TAG_FLASH_ERASE 0xFFF6
#define TAG_FLASH_WRITE 0xFFF7
#define TAG_PLUGCTL     0xFFF8  // loadable plugin control
// 0xFFF9
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

/* TAG_U32
   This is a skeleton protocol with the following structure:
   N, W0, ... WN, rest
   N:    u16_be number of u32 arguments, high bit=endianness: 0=big
   Wx:   u32_be argument
   rest: opaque binary payload
*/

#endif
