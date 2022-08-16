
#include "mod_monitor_3if.c"

int main(int argc, char **argv) {
    struct monitor_3if s;
    struct cbuf out; uint8_t out_buf[256]; CBUF_INIT(out);
    uint8_t ds_buf[256];
    uint8_t rs_buf[256];
    monitor_3if_init(&s, &out, ds_buf, rs_buf);
    uint8_t input[] = {
        1, ACK,
        4, NPUSH,  1, 2, 3,
        2, NPOP,   3,
    };
    monitor_3if_write(&s, input, sizeof(input));
    uint8_t n;
    while (1 == cbuf_read(&out, &n, 1)) {
        LOG("%d", n);
        uint8_t c;
        for (uint8_t i=0; i<n; i++) {
            ASSERT(1 == cbuf_read(&out, &c, 1));
            LOG(" %d", c);
        }
        LOG("\n");
    }
    LOG("\n");
    return 0;
}
