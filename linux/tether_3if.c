#include "mod_tether_3if.c"

#if 1
void test(struct tether *s) {
    // FIXME: Flush read
    tether_ack(s);
    tether_cmd_u32(s, LDA, 0x20000000);
    for(int i=0; i<4; i++) {
        tether_cmd_u8(s, NAL, 16);
    }
    tether_cmd_u32(s, LDF, 0x08000000);
    for(int i=0; i<4; i++) {
        tether_cmd_u8(s, NFL, 16);
    }
}
#endif

int main(int argc, char **argv) {
    struct tether s;
    ASSERT(argc == 2);
    const char *dev = argv[1];
    ASSERT_ERRNO(s.fd = open(dev, O_RDWR));
    raw_serial_config(s.fd);
    test(&s);
    return 0;

}

