#include "mod_ilog_browser.c"

/* Just an example.
   Fill in the message rendering for the specific file format. */

/* Get a string representation of the message at index. */
void ib_format_message(struct ilog_browser *s, int index,
                       char *buf, int max_chars) {

    const uint8_t *msg = ilog_get(&s->ilog, index);
    uint32_t size = read_be(msg,4);
    if (size < 2) {
        sprintf(buf, "%d: bad size %d", index, size);
        return;
    }
    uint16_t tag = read_be(msg+4,2);
    sprintf(buf, "%d: tag 0x%04x, size %d", index, tag, size);
}

int main(int argc, char **argv) {
    if (argc != 2) {
        LOG("usage: %s <ilog>\n", argv[0]);
        exit(1);
    }
    ib_loop(argv[1]);
}
