/* Connect to a 3if monitor. */

/* For debugging, it seems simplest to just let the device dump flash
   at startup.  Then handle whatever interpretation is necessary later
   on. */

#include "macros.h"
#include "raw_serial.h"
#include "assert_write.h"
#include "assert_read.h"

// FIXME: move these to a header
#define MONITOR_3IF_FOR_PRIM(m)                              \
    m(ACK,  0x0)  m(NPUSH, 0x1)  m(NPOP, 0x2)  m(JSR,  0x3)  \
    m(LDA,  0x4)  m(LDF,   0x5)  m(LDC,  0x6)  m(INTR, 0x7)  \
    m(NAL,  0x8)  m(NFL,   0x9)  m(NAS,  0xa)  m(NFS,  0xb)  \

#define PRIM_ENUM_INIT(word,N) word = (0x80 + N),
enum PRIM { MONITOR_3IF_FOR_PRIM(PRIM_ENUM_INIT) };


int fd = -1;
void log_cmd(const uint8_t *buf) {
    uint8_t len = buf[0];
    buf++;
    LOG("(%02x)", len);
    for (uint8_t i=0; i<len; i++) {
        LOG(" %02x", buf[i]);
    }
    LOG("\n");
}
void rpl(uint8_t *buf) {
    uint8_t local_buf[256];
    if (!buf) buf=local_buf;
    assert_read(fd, &buf[0], 1);
    assert_read(fd, buf+1, buf[0]);
    log_cmd(buf);
}
#define WRITE(...) do {                     \
        uint8_t buf[] = { __VA_ARGS__ };    \
        log_cmd(buf);                       \
        assert_write(fd, buf, sizeof(buf)); \
    } while(0)

void u32(uint8_t opc, uint32_t val) {
    WRITE(5, opc, val, val>>8, val>>16, val>>24);
    rpl(NULL);
}
void u8(uint8_t opc, uint8_t val) {
    WRITE(2, opc, val);
    rpl(NULL);
}
void ack(void) { WRITE(1, ACK); rpl(NULL); }

int main(int argc, char **argv) {
    ASSERT(argc == 2);
    const char *dev = argv[1];
    ASSERT_ERRNO(fd = open(dev, O_RDWR));
    raw_serial_config(fd);
    // FIXME: Flush read
    ack();
    u32(LDA, 0x20000000);
    for(int i=0; i<4; i++) {
        u8(NAL, 16);
    }
    u32(LDF, 0x08000000);
    for(int i=0; i<4; i++) {
        u8(NFL, 16);
    }
    return 0;
}

