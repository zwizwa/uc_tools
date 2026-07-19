#define _GNU_SOURCE

// tom@mimas:/i/tom/rdm-bridge/uc_tools/linux$ ./test_log_parse_index.dynamic.host.elf /i/tom/rdm-bridge/uc_trace/console.2023041

#include "log_parse_mmf.h"


static inline void log_parse_mmf_test(const char *filename) {
    struct log_parse_mmf _lpm = {}, *lpm = &_lpm;
    log_parse_mmf_open(lpm, filename);

    // log_parse_mmf_wind(lpm, 864);

    /* Traverse */
    while(!log_parse_mmf_eof(lpm)) {
        if (lpm->lpi.bin) {
            LOG("%08x %4d <bin>\n",
                lpm->ts,
                (int)lpm->lpi.offset);
        }
        else {
            /* Note that lpm->line is not zero terminated and in case
               of binary it includes the newline if there is one. */
            uintptr_t len = lpm->lpi.data_len;
            uint8_t line[len+1];
            uintptr_t data_offset = lpm->lpi.offset + lpm->lpi.data_offset;
            memcpy(line, lpm->log_mmf.buf + data_offset, len);
            line[len] = 0;
            if (line[len-1] == '\n') {
                line[len-1] = 0;
            }
            LOG("%08x %4d '%s'\n",
                lpm->ts,
                (int)lpm->lpi.offset,
                line);

        }
        log_parse_mmf_next(lpm);
    }

    /* Cleanup */
    log_parse_mmf_close(lpm);
}


int main(int argc, char **argv) {
    // Hardcoded test file on carpo.
    const char *filename = "/ssd/enc/c8-logs/new/20260717-060731/t2/usb.002.bin";
    if (argc >= 2) {
        filename = argv[1];
    }
    LOG("log_parse_index %s\n", filename);
    //log_parse_create_index(filename);
    log_parse_mmf_test(filename);
    return 0;
}
