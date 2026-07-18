#ifndef LOG_PARSE_MMF_H
#define LOG_PARSE_MMF_H

/* The log_parse.h code is just the parse + iterate algorithm and
   doesn't rely on any OS functionality.  Here we add:
   - a logfile is a memory-mapped file
   - cursor is explicit (e.g. model after sqlite virtual table ops)
   - timestamps are unrolled
   - later: support index files and rewind (current parser doesn't track offset)
*/

#include "log_parse.h"
#include "mmap_file.h"

#include <inttypes.h>


/* log_parse iterator with mmf attached. */
struct log_parse_mm {
    struct log_parse lp;     // parser state, must be first
    struct log_parse_cbs cb; // parser callbacks
    struct mmap_file mmf;    // memory mapped logfile
    uint32_t hi_ts;          // hi timestamp, rollover counter
    int fd;                  // optional file descriptor

    /* Current log message. */
    uintptr_t msg_offset;    // offset of start of message in mmf
    uintptr_t line_offset;   // offset of line data in mmf buffer
    const uint8_t *line;     // buffered line data inside lp
    intptr_t len;            // length of line data
    uint32_t ts;             // timestamp
    uint8_t bin:1;           // this is a binary message
    uint8_t eof:1;           // no current message, stream at eof condition

};
static log_parse_status_t log_parse_mmf_ts_cb(
    struct log_parse *s, uint32_t ts,
    const uint8_t *line, uintptr_t len, int bin)
{
    //LOG("ts_line_cb 0x%08x %p %d\n", ts, line, len);
    struct log_parse_mm *lpm = (void*)s;

    const uint8_t *buf = lpm->mmf.buf;

    if (ts < lpm->ts) {
        /* Rollover occured.  Note that this requires at least one
           message with a time stamp difference that is smaller than 2^32. */
        lpm->hi_ts++;
    }

    /* Current log entry. */
    lpm->ts   = ts;
    lpm->line = line;
    lpm->len  = len;
    lpm->bin  = bin;

    /* Track the framing. */
    lpm->msg_offset = s->in_start - buf;
    lpm->line_offset = s->in_mark - buf;

    return LOG_PARSE_STATUS_YIELD;
}
static log_parse_status_t log_parse_mmf_ts_line_cb(
    struct log_parse *s, uint32_t ts,
    const uint8_t *line, uintptr_t len) {
    return log_parse_mmf_ts_cb(s,ts,line,len,0);
}
static log_parse_status_t log_parse_mmf_line_cb(
    struct log_parse *s, uint32_t ts,
    const uint8_t *line, uintptr_t len) {
    /* If there is no timestamp, reuse the last one. */
    struct log_parse_mm *lpm = (void*)s;
    return log_parse_mmf_ts_cb(s,lpm->ts,line,len,0);
}
static log_parse_status_t log_parse_mmf_ts_bin_cb(
    struct log_parse *s, uint32_t ts,
    const uint8_t *line, uintptr_t len) {
    return log_parse_mmf_ts_cb(s,ts,line,len,1);
}

static inline void log_parse_mmf_next(struct log_parse_mm *lpm) {
    log_parse_status_t s = log_parse_continue(&lpm->lp);
    lpm->eof = (s != LOG_PARSE_STATUS_YIELD);
}

static inline void log_parse_mmf_open_write_index(
    struct log_parse_mm *lpm, const char *filename)
{
    /* Create the index file. */
    char index_name[strlen(filename) + 6 + 1];
    sprintf(index_name, "%s.index", filename);
    int flags = (O_WRONLY | O_TRUNC | O_CREAT);
    ASSERT_ERRNO(lpm->fd = open(index_name, flags));
}

static inline void log_parse_mmf_open(struct log_parse_mm *lpm, const char *filename) {
    memset(lpm, 0, sizeof(*lpm));
    lpm->fd = -1;

    /* Attach the input file. */
    mmap_file_open_ro(&lpm->mmf, filename);

    /* Initialize the iterator. */
    struct log_parse *lp = &lpm->lp;
    log_parse_init_with(lp, lpm->mmf.buf, lpm->mmf.size);
    lp->cb = &lpm->cb;
    lp->cb->line    = log_parse_mmf_line_cb;
    lp->cb->ts_line = log_parse_mmf_ts_line_cb;
    lp->cb->ts_bin  = log_parse_mmf_ts_bin_cb;

    /* Initialize cursor on the first log entry. */
    log_parse_mmf_next(lpm);
}
static inline void log_parse_mmf_wind(struct log_parse_mm *lpm, uintptr_t offset) {
    struct log_parse *lp = &lpm->lp;
    ASSERT(offset < lpm->mmf.size);
    log_parse_reset(lp, lpm->mmf.buf + offset, lpm->mmf.size - offset);
    /* Initialize cursor on the first log entry. */
    log_parse_mmf_next(lpm);
}
static inline void log_parse_mmf_close(struct log_parse_mm *lpm) {
    if (lpm->fd != -1) close(lpm->fd);
    mmap_file_close(&lpm->mmf);
}
static inline int log_parse_mmf_eof(struct log_parse_mm *lpm) {
    return lpm->eof;
}

static inline void log_parse_create_index(const char *filename) {
    struct log_parse_mm _lpm = {}, *lpm = &_lpm;
    log_parse_mmf_open(lpm, filename);
    log_parse_mmf_open_write_index(lpm, filename);
    /* Traverse */
    while(!log_parse_mmf_eof(lpm)) {
        uint64_t ts    = lpm->ts;
        uint64_t hi_ts = lpm->hi_ts;
        struct log_parse_index idx = {
            .timestamp  = ts + (hi_ts << 32),
            .offset     = lpm->msg_offset,
        };
        assert_write(lpm->fd, (void*)&idx, sizeof(idx));
        log_parse_mmf_next(lpm);
    }
    /* Cleanup */
    log_parse_mmf_close(lpm);
}

static inline void log_parse_mmf_test(const char *filename) {
    struct log_parse_mm _lpm = {}, *lpm = &_lpm;
    log_parse_mmf_open(lpm, filename);

    log_parse_mmf_open_write_index(lpm, filename);

    // log_parse_mmf_wind(lpm, 864);

    /* Traverse */
    while(!log_parse_mmf_eof(lpm)) {
        if (lpm->bin) {
            LOG("%08x %4d <bin>\n",
                lpm->ts,
                (int)lpm->msg_offset);
        }
        else {
            /* Note that lpm->line is not zero terminated and in case
               of binary it includes the newline if there is one. */
            uint8_t line[lpm->len+1];

            if (0) {
                memcpy(line, lpm->line, lpm->len);
            }
            else {
                memcpy(line, lpm->mmf.buf + lpm->line_offset, lpm->len);
            }


            line[lpm->len] = 0;
            if (line[lpm->len-1] == '\n') {
                line[lpm->len-1] = 0;
            }
            LOG("%08x %4d '%s'\n",
                lpm->ts,
                (int)lpm->msg_offset,
                line);

        }
        log_parse_mmf_next(lpm);
    }

    /* Cleanup */
    log_parse_mmf_close(lpm);
}



#endif
