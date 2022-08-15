
#include "mod_monitor_3if.c"

int main(int argc, char **argv) {
    struct monitor_3if s;
    struct cbuf out; uint8_t out_buf[256]; CBUF_INIT(out);
    uint8_t ds_buf[256];
    uint8_t rs_buf[256];
    monitor_3if_init(&s, &out, ds_buf, rs_buf);
    uint8_t input[] = {
        ACK,
        NPUSH, 3,  1, 2, 3,
        NPOP,  3,
    };
    monitor_3if_write(&s, input, sizeof(input));
    uint8_t c;
    while (1 == cbuf_read(&out, &c, 1)) {
        LOG(" %d\n", c);
    }
    LOG("\n");
    return 0;
}
