 /* Connect to a 3if monitor. */

/* For debugging, it seems simplest to just let the device dump flash
   at startup.  Then handle whatever interpretation is necessary later
   on. */

#include "macros.h"
#include "raw_serial.h"
#include "assert_write.h"
#include "assert_read.h"

const char *tether_3if_tag = "";

struct tether {
    int fd;
    /* Max transfer is size byte + max size indicated by that size
       byte (255 bytes) */
    uint8_t buf[256];

    unsigned int verbose:1;
    unsigned int progress:1;
};

// FIXME: move these to a header
#define MONITOR_3IF_FOR_PRIM(m)                              \
    m(ACK,  0x0)  m(NPUSH, 0x1)  m(NPOP, 0x2)  m(JSR,  0x3)  \
    m(LDA,  0x4)  m(LDF,   0x5)  m(LDC,  0x6)  m(INTR, 0x7)  \
    m(NAL,  0x8)  m(NFL,   0x9)  m(NAS,  0xa)  m(NFS,  0xb)  \

#define PRIM_ENUM_INIT(word,N) word = (0x80 + N),
enum PRIM { MONITOR_3IF_FOR_PRIM(PRIM_ENUM_INIT) };


int fd = -1;
void tether_log(struct tether *s, const char *tag) {
    if (!s->verbose) return;
    uint8_t len = s->buf[0];
    LOG("%s(%02x)", tag, len);
    for (uint8_t i=0; i<len; i++) {
        LOG(" %02x", s->buf[i+1]);
    }
    LOG("\n");
}
void tether_clear(struct tether *s) {
    memset(s->buf, 0, sizeof(s->buf));
}
void tether_read(struct tether *s) {
  again:
    tether_clear(s);
    assert_read(s->fd, &s->buf[0], 1);
    if (s->buf[0] == 0) {
        /* This is used for buffer padding. */
        // LOG("skip empty packet\n");
        goto again;
    }
    assert_read(s->fd, &s->buf[1], s->buf[0]);
    tether_log(s, "R: ");
}
void tether_write_flush(struct tether *s) {
    tether_log(s, "W: ");
    assert_write((s)->fd, s->buf, s->buf[0]+1);
}
void tether_write(struct tether *s, uint8_t *buf) {
    /* First byte contains size. */
    tether_clear(s);
    memcpy((s)->buf, buf, buf[0]+1);
    tether_write_flush(s);
}
#define TETHER_WRITE(s, ...) do {                              \
        uint8_t buf[] = { __VA_ARGS__ };                       \
        tether_write(s, buf);                                  \
    } while(0)

void tether_cmd_u32(struct tether *s, uint8_t opc, uint32_t val) {
    TETHER_WRITE(s, 5, opc, val, val>>8, val>>16, val>>24);
    tether_read(s);
}
void tether_cmd_u8(struct tether *s, uint8_t opc, uint8_t val) {
    TETHER_WRITE(s, 2, opc, val);
    tether_read(s);
}
void tether_cmd(struct tether *s, uint8_t opc) {
    TETHER_WRITE(s, 1, opc);
    tether_read(s);
}
void tether_cmd_buf(struct tether *s, uint8_t opc, const uint8_t *buf, uintptr_t size) {
    ASSERT(size <= 254);
    tether_clear(s);
    s->buf[0] = size + 1;
    s->buf[1] = opc,
    memcpy(&s->buf[2], buf, size);
    tether_write_flush(s);
    tether_read(s);
}
void tether_ack(struct tether *s) {
    tether_cmd(s, ACK);
}
void tether_exec(struct tether *s, uint32_t addr) {
    /* Load code register.  FIXME: We're only doing ARM Thumb, so make
       sure the thumb bit is set in the function pointer.  The monitor
       does not do that. */
    addr |= 1;
    tether_cmd_u32(s, LDC, addr);
    /* Jump to subroutine. */
    tether_cmd(s, JSR);
}
void tether_jsr(struct tether *s) {
    tether_cmd(s, JSR);
}


void tether_read_mem(struct tether *s, uint8_t *buf,
                      uint32_t address, uint32_t nb_bytes,
                      enum PRIM LDx, enum PRIM NxL) {

    tether_cmd_u32(s, LDx, address);
    while (nb_bytes > 0) {
        /* FIXME: There's a transfer bug when max_chunk > 127.  Some
           signed/unsigned confusion? */
        uint32_t max_chunk = 127;
        uint32_t chunk = (nb_bytes > max_chunk) ? max_chunk : nb_bytes;
        if (s->verbose) { LOG("%08x %d\n", address, chunk); };
        tether_cmd_u8(s, NxL, chunk);
        ASSERT(s->buf[0] == chunk);
        memcpy(buf, s->buf+1, chunk);
        address += chunk;
        nb_bytes -= chunk;
        buf += chunk;
    }
}
 

void tether_dump_mem(struct tether *s, const char *filename,
                     uint32_t address, uint32_t nb_bytes,
                     enum PRIM LDx, enum PRIM NxL) {
    uint8_t buf[nb_bytes];
    FILE *f;
    ASSERT(f = fopen(filename, "w+"));
    tether_read_mem(s, buf, address, nb_bytes, LDx, NxL);
    ASSERT(nb_bytes == fwrite(buf, 1, nb_bytes, f));
    fclose(f);
}
void tether_dump_flash(struct tether *s, const char *filename,
                       uint32_t address, uint32_t nb_bytes) {
    return tether_dump_mem(s, filename, address, nb_bytes, LDF, NFL);
}
void tether_dump_ram(struct tether *s, const char *filename,
                     uint32_t address, uint32_t nb_bytes) {
    return tether_dump_mem(s, filename, address, nb_bytes, LDA, NAL);
}

void tether_assert_ack(struct tether *s) {
    ASSERT(s->buf[0] == 1);
    ASSERT(s->buf[1] == 0);
}


void tether_write_mem(struct tether *s, const uint8_t *buf,
                      uint32_t address, uint32_t nb_bytes,
                      enum PRIM LDx, enum PRIM NxS) {
    uint32_t total_bytes = nb_bytes;
    tether_cmd_u32(s, LDx, address);
    while (nb_bytes > 0) {
        uint32_t max_chunk = 127;
        uint32_t chunk = (nb_bytes > max_chunk) ? max_chunk : nb_bytes;
        if (s->verbose) { LOG("%08x %d\n", address, chunk); };
        tether_cmd_buf(s, NxS, buf, chunk);
        tether_assert_ack(s);
        address += chunk;
        nb_bytes -= chunk;
        buf += chunk;
        if (s->progress) {
            uint32_t percent = (100 * (total_bytes - nb_bytes)) / total_bytes;
            LOG("\r%d%\r", percent);
        }
    }
    if (s->progress) {
        LOG("\r     \r");
    }
}

int tether_verify(struct tether *s, const uint8_t *buf,
                   uint32_t address, uint32_t in_len,
                   enum PRIM LDx, enum PRIM NxL) {
    uint8_t buf_verify[in_len];
    tether_read_mem(s, buf_verify, address, in_len, LDx, NxL);
    return 0 == memcmp(buf, buf_verify, in_len);
} 
int tether_verify_flash(struct tether *s, const uint8_t *buf,
                        uint32_t address, uint32_t in_len) {
    return tether_verify(s, buf, address, in_len, LDF, NFL);
}


void tether_load(struct tether *s, const char *filename, uint32_t address,
                 enum PRIM LDx, enum PRIM NxS, enum PRIM NxL) {
    /* Load the file from storage. */
    FILE *f_in;
    ASSERT(f_in = fopen(filename, "r"));
    ASSERT(0 == fseek(f_in, 0, SEEK_END));
    uint32_t in_len = ftell(f_in);
    uint8_t buf[in_len];
    ASSERT(0 == fseek(f_in, 0, SEEK_SET));
    ASSERT(in_len == fread(buf, 1, in_len, f_in));
    fclose(f_in);
    if (tether_verify(s, buf, address, in_len, LDx, NxL)) {
        /* Do not write to flash if not needed in order to not wear it
           out on frequent restarts.  Since we're not changing the
           control block this can be done by straight memory
           compare. */
        LOG("%s%08x was loaded\n", tether_3if_tag, address);
        return;
    }
    tether_write_mem(s, buf, address, in_len, LDx, NxS);
    if (!tether_verify(s, buf, address, in_len, LDx, NxL)) {
        LOG("%08x WARNING: verify failed\n", address);
    }
    else { // if (s->verbose)
        LOG("%s%08x verify ok\n", tether_3if_tag, address);
    }
}


void tether_write_flash(struct tether *s, const uint8_t *buf,
                        uint32_t address, uint32_t nb_bytes) {
    /* This is a constraint imposed by the bootloader.  Data will only
       be written in chunks of 64 bytes. */
    ASSERT(0 == (nb_bytes % 64));
    return tether_write_mem(s, buf, address, nb_bytes, LDF, NFS);
}
void tether_write_ram(struct tether *s, const uint8_t *buf,
                      uint32_t address, uint32_t nb_bytes) {
    return tether_write_mem(s, buf, address, nb_bytes, LDA, NAS);
}



void tether_load_flash(struct tether *s, const char *filename, uint32_t address) {
    return tether_load(s, filename, address,\
                       LDF, NFS, NFL);
}
void tether_load_ram(struct tether *s, const char *filename, uint32_t address) {
    return tether_load(s, filename, address,
                       LDA, NAS, NAL);
}






// FIXME
void tether_sync(struct tether *s) {
    uint8_t buf[256] = {};
    int rv;
    /* Read any stale replies with non-blocking set temporarily. */
    int opt;
    ASSERT_ERRNO(opt = fcntl(s->fd, F_GETFL));
    opt |= O_NONBLOCK;
    ASSERT_ERRNO(fcntl(s->fd, F_SETFL, opt));
    ASSERT_ERRNO(rv = read(s->fd, buf, sizeof(buf)));
    opt &= ~O_NONBLOCK;
    ASSERT_ERRNO(fcntl(s->fd, F_SETFL, opt));

    LOG("sync1: %d\n", rv);

    /* Send a bunch of zeros to recover from protocol sync loss.  This
       finalizes any command that is in progress and will then fall
       into skipping empty packets. */
    write(s->fd, buf, sizeof(buf));

    /* Read again, this time blocking. */
    rv = read(s->fd, buf, sizeof(buf));
    LOG("sync2: %d\n", rv);

}
void tether_open_tty(struct tether *s, const char *dev) {
    ASSERT_ERRNO(s->fd = open(dev, O_RDWR));
    raw_serial_config(s->fd);
    // tether_sync(s);
}


