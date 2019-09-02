#ifndef SLIP_H
#define SLIP_H


/* SLIP */
#define SLIP_END     0xC0 // 192 Packet separation marker (Break/MAB start)
#define SLIP_ESC     0xDB // 219 Escape character
#define SLIP_ESC_END 0xDC // 220 Re-mapped END, after ESC
#define SLIP_ESC_ESC 0xDD // 221 Re-mapped ESC, after ESC

#define CBUF_OOB_BASE 0x200

/* SLIP is 8-bit data plus an out-of-band END code.  We can use the
 * escape mechanism to represent some more OOB codes. This is a macro
 * so it can be used as a "case" */
#define CBUF_OOB(code) (CBUF_OOB_BASE | ((code) & 0xFF))


/* SLIP by itself is just a packet wrapper.  In any practical
   situation two more elements are necessary:

   - root level type tags to be able to send different packet types

   - some level of redundancy to ensure that the root level tags are
     even correct

   The "system tags" are defined here.

   Two bytes are used, since I have this gut feeling that one byte is
   just not enough for most cases.  Use network order.  System tags
   are 0xFF__, the rest is for the application.

   As for checksums, there are conflicting requirements.  On one hand
   we would like to just provide a reliable transport mechanism, on
   the other hand, many formats already have consistency checks, and
   it will be hard to find a good one size fits all.  So we don't
   bother.

*/

// FIXME: These are not slip-specific, just packet dispatch.

#define TAG_PING   0xFFFC  // request TAG_REPLY echo
#define TAG_GDB    0xFFFD  // gdbstub RSP tunnel
#define TAG_INFO   0xFFFE  // info log data
#define TAG_REPLY  0xFFFF  // standard reply/ack format

#endif
