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
struct log_parse_mmf {
    struct log_parse lp;        // parser state, must be first
    struct log_parse_cbs cb;    // parser callbacks
    struct mmap_file log_mmf;   // memory mapped logfile
    struct mmap_file index_mmf; // memory mapped index
    uint32_t hi_ts;             // hi timestamp, rollover counter
    int index_fd;

    /* Current log message. */
    uintptr_t msg_nb;        // offset of start of message in mmf
    uintptr_t msg_offset;    // offset of start of message in mmf
    uintptr_t line_offset;   // offset of line data in mmf buffer
    const uint8_t *line;     // buffered line data inside lp
    uintptr_t len;           // length of line data
    uint32_t ts;             // timestamp
    uint32_t group;          // last log marker / message group id
    uint8_t bin:1;           // this is a binary message
    uint8_t eof:1;           // no current message, stream at eof condition
    uint8_t open:1;          // for idempotent close

};

static inline const struct log_parse_index *log_parse_mmf_index(struct log_parse_mmf *lpm) {
    return lpm->index_mmf.buf;
}
static inline const char *log_parse_mmf_log(struct log_parse_mmf *lpm) {
    return lpm->log_mmf.buf;
}

static inline intptr_t log_parse_mmf_index_size(struct log_parse_mmf *lpm) {
    const struct log_parse_index *idx = log_parse_mmf_index(lpm);
    if (!idx) return -1; // Error: there is no index, only linear traversal.
    return lpm->index_mmf.size / sizeof(*idx);
}


static log_parse_status_t log_parse_mmf_ts_cb(
    struct log_parse *s, uint32_t ts,
    const uint8_t *line, uintptr_t len, int bin)
{
    //LOG("ts_line_cb 0x%08x %p %d\n", ts, line, len);
    struct log_parse_mmf *lpm = (void*)s;

    const uint8_t *buf = lpm->log_mmf.buf;

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

    /* Track logmark messages */
    if (bin && (len == 5) && (line[0] == 0)) {
        lpm->group = read_le(line+1, 4);
        //LOG("logmark = %d\n", (int32_t)lpm->group);
        //for (int i=0; i<len; i++) { LOG(" %02x", line[i]); } LOG("\n");
    }

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
    struct log_parse_mmf *lpm = (void*)s;
    return log_parse_mmf_ts_cb(s,lpm->ts,line,len,0);
}
static log_parse_status_t log_parse_mmf_ts_bin_cb(
    struct log_parse *s, uint32_t ts,
    const uint8_t *line, uintptr_t len) {
    return log_parse_mmf_ts_cb(s,ts,line,len,1);
}

static inline void log_parse_mmf_next(struct log_parse_mmf *lpm) {
    log_parse_status_t s = log_parse_continue(&lpm->lp);
    lpm->msg_nb++;
    lpm->eof = (s != LOG_PARSE_STATUS_YIELD);
}
static inline void log_parse_mmf_first(struct log_parse_mmf *lpm) {
    /* Make sure first element or eof condition is loaded. */
    log_parse_mmf_next(lpm);
    /* Correct the offset for the first message.  In the _next()
       routine we can only pre-increment. */
    lpm->msg_nb = 0;
}

static inline void log_parse_mmf_create_or_open_index(
    struct log_parse_mmf *mmf, const char *filename);

static inline void log_parse_mmf_init_iterator(struct log_parse_mmf *lpm) {

    /* Initialize the iterator. */
    struct log_parse *lp = &lpm->lp;
    log_parse_init_with(lp, lpm->log_mmf.buf, lpm->log_mmf.size);
    lp->cb = &lpm->cb;
    lp->cb->line    = log_parse_mmf_line_cb;
    lp->cb->ts_line = log_parse_mmf_ts_line_cb;
    lp->cb->ts_bin  = log_parse_mmf_ts_bin_cb;

    /* Initialize cursor on the first log entry. */
    log_parse_mmf_first(lpm);
}

static inline void log_parse_mmf_open_opts(
    struct log_parse_mmf *lpm, const char *filename, int use_index)
{
    memset(lpm, 0, sizeof(*lpm));
    lpm->open = 1;
    lpm->index_fd = -1;

    /* Attach the input file. */
    mmap_file_open_ro(&lpm->log_mmf, filename);

    /* Try to open the index file first. */
    if (use_index) {
        log_parse_mmf_create_or_open_index(lpm, filename);
        if (lpm->index_fd != -1) {
            mmap_file_open_ro_fd(&lpm->index_mmf, lpm->index_fd);
            lpm->index_fd = -1; /* call consumed fd */
        }
    }

    log_parse_mmf_init_iterator(lpm);
}
static inline void log_parse_mmf_open(struct log_parse_mmf *lpm, const char *filename) {
    log_parse_mmf_open_opts(lpm, filename, 1 /* create index */);
}


static inline void log_parse_mmf_wind(struct log_parse_mmf *lpm, uintptr_t offset) {
    struct log_parse *lp = &lpm->lp;
    ASSERT(offset < lpm->log_mmf.size);
    log_parse_reset(lp, lpm->log_mmf.buf + offset, lpm->log_mmf.size - offset);
    /* Initialize cursor on the first log entry. */
    log_parse_mmf_first(lpm);
}
static inline void log_parse_mmf_close(struct log_parse_mmf *lpm) {
    /* This needs to be idempotent: once opened, closed, closing it
       after that should be a no-op.  It should work for a zero-filled
       struct as well. */
    if (!lpm->open) return;
    if (lpm->index_fd != -1) close(lpm->index_fd);
    mmap_file_close(&lpm->log_mmf);
    mmap_file_close(&lpm->index_mmf);

    /* Re-initialize as closed. */
    memset(lpm, 0, sizeof(*lpm));
}
static inline int log_parse_mmf_eof(struct log_parse_mmf *lpm) {
    return (!lpm->open) || lpm->eof;
}

static inline uint64_t log_parse_mmf_timestamp(struct log_parse_mmf *lpm) {
    uint64_t ts    = lpm->ts;
    uint64_t hi_ts = lpm->hi_ts;
    return ts + (hi_ts << 32);
}

/* Create index file in open state. */
static inline void log_parse_mmf_create_or_open_index(
    struct log_parse_mmf *lpm, const char *filename)
{

    /* Derive index filename. */
    char index_name[strlen(filename) + 6 + 1];
    sprintf(index_name, "%s.index", filename);


    /* Attempt to open it. */
    lpm->index_fd = open(index_name, O_RDONLY, 0664);

    if (-1 != lpm->index_fd) {
        /* Postcondition: index file is opened readonly. */
        return;
    }

    /* That didn't work.  Attempt to recreate. */
    LOG("create index %s\n", index_name);
    int flags = (O_WRONLY | O_TRUNC | O_CREAT);
    ASSERT_ERRNO(lpm->index_fd = open(index_name, flags, 0664));

    /* This is called as part of _open_opts() and the iterator is not
       yet initialized. */
    log_parse_mmf_init_iterator(lpm);

    /* Traverse */
    while(!log_parse_mmf_eof(lpm)) {
        struct log_parse_index idx = {
            .timestamp   = log_parse_mmf_timestamp(lpm),
            .offset      = lpm->msg_offset,
            .group       = lpm->group,
            .data_len    = lpm->len,
            .data_offset = (lpm->line_offset - lpm->msg_offset),
            .bin         = lpm->bin,
            .sync        = lpm->bin && (lpm->len == 5) && (lpm->line[0] == 0),
        };
        /* FIXME: This will benefit from buffering. */
        assert_write(lpm->index_fd, (void*)&idx, sizeof(idx));
        log_parse_mmf_next(lpm);
    }

    /* Close the file and reopen read-only. */
    close(lpm->index_fd);
    ASSERT_ERRNO(lpm->index_fd = open(index_name, O_RDONLY, 0664));
    /* Postcondition: index file is opened readonly. */

    // FIXME: rewind
}

/* Wrapper that just creates the index file. */
static inline void log_parse_create_index(const char *filename) {
    // FIXME: Delete the index file
    /* Index is created as a side effect of opening the file. */
    struct log_parse_mmf _lpm = {}, *lpm = &_lpm;
    log_parse_mmf_open(lpm, filename);
    log_parse_mmf_close(lpm);
}

static inline void log_parse_mmf_test(const char *filename) {
    struct log_parse_mmf _lpm = {}, *lpm = &_lpm;
    log_parse_mmf_open(lpm, filename);

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
                memcpy(line, lpm->log_mmf.buf + lpm->line_offset, lpm->len);
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
