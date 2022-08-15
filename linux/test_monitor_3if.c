
#include "mod_monitor_3if.c"

int main(int argc, char **argv) {
    struct monitor_3if s;
    monitor_3if_init(&s);
    uint8_t input[] = {1,2,3};
    monitor_3if_write(&s, input, sizeof(input));
    uint8_t c;
    while (1 == cbuf_read(&s.out, &c, 1)) {
        LOG(" %d\n", c);
    }
    LOG("\n");
    return 0;
}
