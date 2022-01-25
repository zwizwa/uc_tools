/* Parser (lexer) for ad-hoc log format.

   Situation is this:

   1. app uses uc_tools packet protocol, with FFFE or FFFB,<index> log
   tags containing a stream of textual log messages.

   2. app runs into performance issues with logging.  a binary
   protocol is embedded inside the text protocol.

   3. things get solidified and messy enough for a small library that
   can disentangle everything and produce structured log outputs + be
   robust against missing data due to log buffer overflows.

   Library is defined in "push_style" and inline functions to make it
   low friction.

*/

// FIXME: We're relying on lib.a library code so maybe just put these
// functions in the lib instead?

#include "tools.h"
#include "uct_byteswap.h"


#ifndef LOG_PARSE_H
#define LOG_PARSE_H

/* By default binary log timestamps are little endian. */
#ifndef LOG_PARSE_SWAP_U32
#define LOG_PARSE_SWAP_U32(x) SWAP_U32(x)
#endif

#ifndef LOG_PARSE_MAX_LINE_LEN
#define LOG_PARSE_MAX_LINE_LEN 1024
#endif

typedef uintptr_t log_parse_status_t;
#define LOG_PARSE_STATUS_CONTINUE 0
#define LOG_PARSE_STATUS_END 1
#define LOG_PARSE_STATUS_YIELD 2

struct log_parse;

typedef log_parse_status_t (*log_parse_cb)(
    struct log_parse *, uint32_t, const uint8_t *, uintptr_t);

/* A collection of callbacks is useful as an object in itself (a
   representation of the meaning/interpretation of a stream), so
   separate it out. */
struct log_parse_cbs {
    log_parse_cb line;
    log_parse_cb ts_line;
    log_parse_cb ts_bin;
    log_parse_cb overflow;
};

struct log_parse {
    void *next;
    uint8_t line[LOG_PARSE_MAX_LINE_LEN];
    uintptr_t line_len;
    uintptr_t bin_len;
    const uint8_t *in;
    const uint8_t *in_mark;  /* only valid for stable *in */
    uintptr_t in_len;
    struct log_parse_cbs cb;
    uintptr_t nb;
};

/* Implemented as a coroutine using computed goto. */

/* FIXME: Optimization: In case the underlying storage is stable, we
   don't need to copy anything to the line buffer. */

/* Character will be in 'c' variable. */
#define LOG_PARSE_GETC(s) \
    { __label__ resume; s->next = &&resume; return status; resume:; }

static inline log_parse_status_t log_parse_tick(struct log_parse *s, uint8_t c) {
    s->nb++;
    log_parse_status_t status = LOG_PARSE_STATUS_CONTINUE;
    if (s->next) goto *s->next;

    /* Default protocol is lines of ASCII text (all chars < 128) */
  read_line:
    // LOG("rl %d %p\n", s->nb, s->in);
    s->in_mark = s->in;
    s->line_len = 0;
    for(;;) {
        LOG_PARSE_GETC(s);
        if (c >= 0x80) {
            goto read_bin;
        }
        if (s->line_len < LOG_PARSE_MAX_LINE_LEN) {
            s->line[s->line_len++] = c;
        }
        if (c == '\n') {
            __label__ done;
            /* Line is ready.  Use fallthrough to hand it over to the
               most specific callback first. */

            /* 32-bit hex time-stamped line. */
            if (s->cb.ts_line) {
                __label__ abort;
                if (s->line_len < 0) goto abort;
                if (s->line[8] != ' ') goto abort;
                // hex_to_bin() doesn't check the nibbles are
                // valid, so we do that separately.
                for (uintptr_t i=0; i<8; i++) {
                    if (-1 == hex_char2int_check(s->line[i])) { goto abort; }
                }
                uint8_t buf[4];
                hex_to_bin(s->line, buf, 4);
                uint32_t ts = read_be(buf, 4);
                s->in_mark += 9;
                status = s->cb.ts_line(s, ts, s->line+9, s->line_len-9);
                goto done;
              abort:;
            }
            /* line without timestamp, or failed timestamp parse */
            if (s->cb.line) {
                status = s->cb.line(s, 0, s->line, s->line_len);
                goto done;
            }
            /* fallthrough */
          done:
            goto read_line;
        }

    }
    /* Binary messages consist of tag byte containing size, 4 bytes of
       32 bit big endian rolling time stamp + max 127 payload
       bytes. */
  read_bin:
    /* FIXME: Spill if there is data? */
    s->in_mark = s->in;
    if (c == 0xFF) {
        /* Overflow character. */
        if (s->cb.overflow) {
            status = s->cb.overflow(s, 0, NULL, 0);
        }
        goto read_line;
    }
    s->bin_len = c - 0x80 + 4;
    s->line_len = 0;
    while(s->line_len < s->bin_len) {
        LOG_PARSE_GETC(s);
        s->line[s->line_len++] = c;
        // LOG("%d %d %02x\n", s->line_len, s->bin_len, c);
    }
    /* Timestamp is in host order, which is currently set to little
       endian. */
    uint32_t ts =  LOG_PARSE_SWAP_U32(read_be(s->line, 4));
    if (s->cb.ts_bin) {
        s->in_mark += 4;
        status = s->cb.ts_bin(s, ts, s->line+4, s->line_len-4);
    }
    goto read_line;

}
static inline log_parse_status_t log_parse_continue(struct log_parse *s) {
    log_parse_status_t status;
    while(s->in_len > 0) {
        status = log_parse_tick(s, *s->in);
        s->in++;
        s->in_len--;
        /* Callbacks can issue stop conditions. */
        if (status != LOG_PARSE_STATUS_CONTINUE) return status;
    }
    /* Stop condition = end of input data. */
    return LOG_PARSE_STATUS_END;
}
static inline log_parse_status_t log_parse_write(
    struct log_parse *s, const uint8_t *buf, uintptr_t len)
{
    s->in = buf;
    s->in_len = len;
    return log_parse_continue(s);
}

static inline void log_parse_write_cstring(struct log_parse *s, const char *str) {
    uintptr_t len = strlen(str);
    log_parse_write(s, (const uint8_t *)str, len);
}

static inline void log_parse_init(struct log_parse *s, const uint8_t *stable_in) {
    memset(s, 0, sizeof(*s));
    // run up to the first read.
    log_parse_tick(s, 0);

    // associate parser to stable input.  in this mode the ->in_mark
    // can be used, but we need to patch it here because starting the
    // machine will have set it to zero.
    if (stable_in) {
        s->in_mark = stable_in;
    }
}

#endif
