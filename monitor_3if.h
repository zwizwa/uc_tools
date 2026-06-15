#ifndef MONITOR_3IF_H
#define MONITOR_3IF_H

/* Same encoding as Staapl.
   A = rAm pointer
   F = Flash pointer
   C = Code pointer
*/

/* Codes are mapped to 0x8x.  This moves it away from:
   - 7-bit clean ASCII
   - SLIP encoding
   - low byte count {packet,N} messages

   This ensures that protocol errors can be used to cause transparent
   protocol switches for things based on. */

#define MONITOR_3IF_FOR_PRIM(m)                              \
    m(ACK,  0x0)  m(NPUSH, 0x1)  m(NPOP, 0x2)  m(JSR,  0x3)  \
    m(LDA,  0x4)  m(LDF,   0x5)  m(LDC,  0x6)  m(INTR, 0x7)  \
    m(NAL,  0x8)  m(NFL,   0x9)  m(NAS,  0xa)  m(NFS,  0xb)  \
    m(BCK,  0xc)


#define PRIM_ENUM_INIT(word,N) word = (0x80 + N),
enum PRIM { MONITOR_3IF_FOR_PRIM(PRIM_ENUM_INIT) };





#endif
