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


#ifndef LOG_PARSE_H
#define LOG_PARSE_H

/* By default binary log timestamps are little endian. */
#ifndef LOG_PARSE_SWAP_U32
#define LOG_PARSE_SWAP_U32(x) SWAP_U32(x)
#endif

#ifndef LOG_PARSE_MAX_LINE_LEN
#define LOG_PARSE_MAX_LINE_LEN 1024
#endif

struct log_parse {
    void *next;
    uint8_t line[LOG_PARSE_MAX_LINE_LEN];
    uintptr_t len;
    uintptr_t bin_len;
    void (*line_cb)   (struct log_parse *, const uint8_t *, uintptr_t);
    void (*ts_line_cb)(struct log_parse *, uint32_t, const uint8_t *, uintptr_t);
    void (*bin_cb)    (struct log_parse *, uint32_t, const uint8_t *, uintptr_t);

};

/* Implemented as a coroutine using computed goto. */

/* Character will be in 'c' variable. */
#define LOG_PARSE_GETC(s) \
    { __label__ resume; s->next = &&resume; return; resume:; }

static inline void log_parse_tick(struct log_parse *s, uint8_t c) {
    if (s->next) goto *s->next;

    /* Default protocol is lines of ASCII text (all chars < 128) */
  read_line:
    s->len = 0;
    for(;;) {
        LOG_PARSE_GETC(s);
        if (c >= 0x80) {
            goto read_bin;
        }
        if (s->len < LOG_PARSE_MAX_LINE_LEN) {
            s->line[s->len++] = c;
        }
        if (c == '\n') {
            /* Line is ready.  Use fallthrough to hand it over to the
               most specific callback first. */

            /* 32-bit hex time-stamped line. */
            if (s->ts_line_cb) {
                __label__ abort;
                if (s->len < 0) goto abort;
                if (s->line[8] != ' ') goto abort;
                // hex_to_bin() doesn't check the nibbles are
                // valid, so we do that separately.
                for (uintptr_t i=0; i<8; i++) {
                    if (-1 == hex_char2int_check(s->line[i])) { goto abort; }
                }
                uint8_t buf[4];
                hex_to_bin(s->line, buf, 4);
                uint32_t ts = read_be(buf, 4);
                s->ts_line_cb(s, ts, s->line+9, s->len-9);
                goto read_line;
              abort:;
            }
            /* normal line. */
            if (s->line_cb) {
                s->line_cb(s, s->line, s->len);
                goto read_line;
            }
            /* fallthrough */
            goto read_line;
        }

    }
    /* Binary messages consist of tag byte containing size, 4 bytes of
       32 bit big endian rolling time stamp + max 127 payload
       bytes. */
  read_bin:
    s->bin_len = c - 0x80 + 4;
    s->len = 0;
    while(s->len < s->bin_len) {
        LOG_PARSE_GETC(s);
        s->line[s->len++] = c;
        // LOG("%d %d %02x\n", s->len, s->bin_len, c);
    }
    /* Timestamp is in host order, which is currently set to little
       endian. */
    uint32_t ts =  LOG_PARSE_SWAP_U32(read_be(s->line, 4));
    if (s->bin_cb) {
        s->bin_cb(s, ts, s->line+4, s->len-4);
    }
    goto read_line;

}
static inline void log_parse_write(struct log_parse *s, const uint8_t *buf, uintptr_t len) {
    while(len > 0) {
        log_parse_tick(s, *buf++);
        len--;
    }
}
static inline void log_parse_write_cstring(struct log_parse *s, const char *str) {
    uintptr_t len = strlen(str);
    log_parse_write(s, (const uint8_t *)str, len);
}

static inline void log_parse_init(struct log_parse *s) {
    memset(s, 0, sizeof(*s));
    log_parse_tick(s, 0); // run up to the first read.
}


#endif
