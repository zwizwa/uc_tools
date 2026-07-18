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
    struct log_parse lp;
    struct mmap_file mmf;
    uint64_t timestamp;
    uint32_t last_u32_timestamp;
    int fd;

    /* Current log message. */
    uintptr_t offset;
    const uint8_t *line;
    intptr_t len;
    uint32_t ts;
    uint8_t bin:1;
    uint8_t eof:1;

};
static log_parse_status_t log_parse_mmf_ts_cb(
    struct log_parse *s, uint32_t ts,
    const uint8_t *line, uintptr_t len, int bin)
{
    //LOG("ts_line_cb 0x%08x %p %d\n", ts, line, len);
    struct log_parse_mm *idx = (void*)s;

    const uint8_t *buf = idx->mmf.buf;

    /* Current log entry. */
    idx->ts   = ts;
    idx->line = line;
    idx->len  = len;
    idx->bin  = bin;

    /* Track the framing.
       FIXME: This doesn't work correctly. */
    //idx->start_offset = idx->endx_offset;
    //idx->endx_offset = s->in - buf;
    // LOG("in_read_line = %p\n", s->in_read_line);

    idx->offset = s->in_mark - buf;
    if (s->in_read_line != buf) {
        // FIXME: Fix the fencepost error elsewhere
        // idx->offset++;
    }

    return LOG_PARSE_STATUS_YIELD;
}
static log_parse_status_t log_parse_mmf_ts_line_cb(
    struct log_parse *s, uint32_t ts,
    const uint8_t *line, uintptr_t len) {
    return log_parse_mmf_ts_cb(s,ts,line,len,0);
}
static log_parse_status_t log_parse_mmf_ts_bin_cb(
    struct log_parse *s, uint32_t ts,
    const uint8_t *line, uintptr_t len) {
    return log_parse_mmf_ts_cb(s,ts,line,len,1);
}

static inline void log_parse_mmf_next(struct log_parse_mm *idx) {
    log_parse_status_t s = log_parse_continue(&idx->lp);
    idx->eof = (s != LOG_PARSE_STATUS_YIELD);
}

static inline void log_parse_mmf_open_write_index(
    struct log_parse_mm *idx, const char *filename)
{
    /* Create the index file. */
    char index_name[strlen(filename) + 6 + 1];
    sprintf(index_name, "%s.index", filename);
    int flags = (O_WRONLY | O_TRUNC | O_CREAT);
    ASSERT_ERRNO(idx->fd = open(index_name, flags));
}

static inline void log_parse_mmf_open(struct log_parse_mm *idx, const char *filename) {
    memset(idx, 0, sizeof(*idx));
    idx->fd = -1;

    /* Attach the input file. */
    mmap_file_open_ro(&idx->mmf, filename);

    /* Initialize the iterator. */
    struct log_parse *lp = &idx->lp;
    log_parse_init_with(lp, idx->mmf.buf, idx->mmf.size);
    struct log_parse_cbs cbs = {
        .line    = log_parse_mmf_ts_line_cb,
        .ts_line = log_parse_mmf_ts_line_cb,
        .ts_bin  = log_parse_mmf_ts_bin_cb,
    };
    lp->cb = &cbs;

    /* Initialize cursor on the first log entry. */
    log_parse_mmf_next(idx);

}
static inline void log_parse_mmf_close(struct log_parse_mm *idx) {
    if (idx->fd != -1) close(idx->fd);
    mmap_file_close(&idx->mmf);
}
static inline int log_parse_mmf_eof(struct log_parse_mm *idx) {
    return idx->eof;
}

static inline void log_parse_create_index(const char *filename) {
    struct log_parse_mm _idx = {}, *idx = &_idx;
    log_parse_mmf_open(idx, filename);

    log_parse_mmf_open_write_index(idx, filename);

    /* Traverse */
    while(!log_parse_mmf_eof(idx)) {
        // FIXME: Save to index

        /* Zero terminate the string. */
        /* FIXME: Handle binary differently. */

        if (idx->bin) {
            LOG("%08x %4d <bin>\n",
                idx->ts,
                (int)idx->offset);
        }
        else {
            /* Note that idx->line is not zero terminated and in case
               of binary it includes the newline if there is one. */
            uint8_t line[idx->len+1];

            if (0) {
                memcpy(line, idx->line, idx->len);
            }
            else {
                memcpy(line, idx->mmf.buf + idx->offset, idx->len);
            }


            line[idx->len] = 0;
            if (line[idx->len-1] == '\n') {
                line[idx->len-1] = 0;
            }
            LOG("%08x %4d '%s'\n",
                idx->ts,
                (int)idx->offset,
                line);

            log_parse_mmf_next(idx);
        }
    }

    /* Cleanup */
    log_parse_mmf_close(idx);
}



#endif
