/* Connect to a 3if monitor. */

/* For debugging, it seems simplest to just let the device dump flash
   at startup.  Then handle whatever interpretation is necessary later
   on. */

#include "macros.h"
#include "raw_serial.h"
#include "assert_write.h"
#include "assert_read.h"
#include "tcp_tools.h"

const char *tether_3if_tag = "";

struct tether;
struct tether {
    int fd_in, fd_out;

    ssize_t (*read)(struct tether *s, void *vbuf, size_t nb);
    void (*write)(struct tether *s, const uint8_t *buf, size_t len);

    /* If set to 0 (default), then use the code default, which
       currently uses maximum allowed size. */
    uint8_t max_chunk_write;

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
    s->read(s, &s->buf[0], 1);
    if (s->buf[0] == 0) {
        /* This is used for buffer padding. */
        if (s->verbose) {
            LOG("skip empty packet\n");
        }
        goto again;
    }
    s->read(s, &s->buf[1], s->buf[0]);
    tether_log(s, "R: ");
}

void tether_write_flush(struct tether *s) {
    tether_log(s, "W: ");
    s->write(s, s->buf, s->buf[0]+1);
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

/* Flush any async messages. */
void tether_flush(struct tether *s, void (*handle)(struct tether *)) {
    TETHER_WRITE(s, 1, ACK);
    for(;;) {
        tether_read(s);
        if ((s->buf[0] == 1) &&
            (s->buf[1] == 0)) break;
        handle(s);
    }
}

/* Note that JSR and INTR need verbatim code pointers in the code
   registers, which on ARM Thumb need to have their LSB set.  Since we
   now also support xtensa, this can't be done here.  Move that LSB=1
   all the way up to the target-specific scripts. */

void tether_exec(struct tether *s, uint32_t addr) {
    /* Load code register. */
    tether_cmd_u32(s, LDC, addr);
    /* Jump to subroutine. */
    tether_cmd(s, JSR);
}

void tether_intr(struct tether *s, uint32_t addr) {
    /* Load code register. */
    tether_cmd_u32(s, LDC, addr);
    /* Jump to subroutine. */
    tether_cmd(s, INTR);
}

void tether_jsr(struct tether *s) {
    tether_cmd(s, JSR);
}

void tether_read_mem(struct tether *s, uint8_t *buf,
                      uint32_t address, uint32_t nb_bytes,
                      enum PRIM LDx, enum PRIM NxL) {

    tether_cmd_u32(s, LDx, address);
    while (nb_bytes > 0) {
        /* 255 is max here, but 254 avoids padding, and this way the
           size is the same as write, which needs an extra command
           byte. */
        uint32_t max_chunk = 254;
        uint32_t chunk = (nb_bytes > max_chunk) ? max_chunk : nb_bytes;
        if (s->verbose) { LOG("%08x %d\n", address, chunk); };
        tether_cmd_u8(s, NxL, chunk);
        if (s->buf[0] != chunk) {
            ERROR("read_mem %d: s->buf[0] == %d, expected %d\n",
                  nb_bytes, s->buf[0], chunk);
        }
        memcpy(buf, s->buf+1, chunk);
        address += chunk;
        nb_bytes -= chunk;
        buf += chunk;
    }
}

uint32_t tether_read_flash_u32(struct tether *s, uint32_t address) {
    uint32_t word;
    tether_read_mem(s, (void*)&word, address, sizeof(word), LDF, NFL);
    return word;
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
        /* 255 is max packet size, 1 for command. */
        uint32_t max_chunk = s->max_chunk_write;
        if (!max_chunk) max_chunk = 254;

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
    for (uint32_t i=0; i<in_len; i++) {
        if (buf[i] != buf_verify[i]) {
            // LOG("%08x diff w=%02x v=%02x\n", address + i, buf[i], buf_verify[i]);
            return 0;
        }
    }
    return 1;

    // return 0 == memcmp(buf, buf_verify, in_len);
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
        LOG("%08x WARNING: verify failed\n");
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
    ASSERT_ERRNO(opt = fcntl(s->fd_in, F_GETFL));
    opt |= O_NONBLOCK;
    ASSERT_ERRNO(fcntl(s->fd_in, F_SETFL, opt));
    ASSERT_ERRNO(rv = read(s->fd_in, buf, sizeof(buf)));
    opt &= ~O_NONBLOCK;
    ASSERT_ERRNO(fcntl(s->fd_in, F_SETFL, opt));

    LOG("sync1: %d\n", rv);

    /* Send a bunch of zeros to recover from protocol sync loss.  This
       finalizes any command that is in progress and will then fall
       into skipping empty packets. */
    assert_write(s->fd_out, buf, sizeof(buf));

    /* Read again, this time blocking. */
    rv = read(s->fd_in, buf, sizeof(buf));
    LOG("sync2: %d\n", rv);

}

ssize_t tether_assert_read_fixed(struct tether *s, void *vbuf, size_t nb) {
    return assert_read_fixed(s->fd_in, vbuf, nb);
}
void tether_assert_write(struct tether *s, const uint8_t *buf, size_t len) {
    assert_write(s->fd_out, buf, len);
}

// Constructor
void tether_open_tty(struct tether *s, const char *dev) {
    memset(s, 0, sizeof(*s));
    ASSERT_ERRNO(s->fd_in = open(dev, O_RDWR));
    s->fd_out = s->fd_in;
    raw_serial_config(s->fd_in);
    s->read  = tether_assert_read_fixed;
    s->write = tether_assert_write;
}


void tether_open_tcp(struct tether *s, const char *host, uint16_t port) {
    memset(s, 0, sizeof(*s));
    s->fd_in = assert_tcp_connect(host, port);
    s->fd_out = s->fd_in;
    s->read  = tether_assert_read_fixed;
    s->write = tether_assert_write;
}

