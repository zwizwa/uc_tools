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

#define TAG_PTERM       0xFFEE  // Erlang printed term
#define TAG_MIDI        0xFFEF  // midi message
#define TAG_LEB128      0xFFF0  // see leb128.erl
#define TAG_COMMAND     0xFFF1  // command.h Forth-style commands
#define TAG_CSP         0xFFF2  // CSP packet
#define TAG_EVENT       0xFFF3  // generic event, uint16_t subtag
#define TAG_RESET       0xFFF4  // reset board
#define TAG_U32         0xFFF5  // skeleton for flat u32 commands
#define TAG_FLASH_ERASE 0xFFF6
#define TAG_FLASH_WRITE 0xFFF7
#define TAG_PLUGCTL     0xFFF8  // loadable plugin control
#define TAG_CHECKSUM    0xFFF9  // compute checksum
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

/* TAG_MIDI
   u16_be: midi port ID
   rest:   parsed midi message
*/

/* TAG_EVENT
   u16_be: event ID:  0000 = DHT11
   rest:   byte stream data
*/

/* TAG_U32
   This is a skeleton "path based" message protocol with the following structure:
   NR, NW, R_0, ..., R_NR-1, A_0, ... A_NA-1, rest
   NR:   u8 number of u32 reply tags (0 for uni-directional messages, nonzero for RPC)
   NA:   u8 number of u32 arguments (which encodes path + payload)
   Wx:   u32_be argument
   rest: opaque binary payload
*/

/* TAG_COMMAND

   Same format as TAG_U32, but reserved for funneling commands and
   arguments to command.h style debug commands.  The TAG_U32 are
   loaded on the stack, then the byte payload is interpreted as a text
   command and executed.   The NR is currently not used. */


/* Reserved tags and ranges:

   0x4xxx IPv4 standard slip
   0x6xxx IPv6 standard slip

   This makes the protocol compatible with "slattach" on Linux.
   Note however that it is not very convenient to do so.

   If encapsulation is necessary, it makes more sense to dedicate an
   Ethernet segment and wrap the protocol as Ethernet.  To allow for
   this, we also reserve the standard EtherTypes:

   https://en.wikipedia.org/wiki/EtherType

   Other projects using this protocol:

   0xC8xx, 0xC9xx, 0x2D2D // C8


*/

/* TAG_REPLY implements RPC replies.
   This works as follows:

   A request can embed an opaque return address.  A reply is then
   constructed by appending TAG_REPLY, the return address, and any
   payload message.
*/

#endif
