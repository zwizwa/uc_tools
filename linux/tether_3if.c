#include "mod_tether_3if.c"

int main(int argc, char **argv) {
    struct tether s;
    ASSERT(argc == 2);
    const char *dev = argv[1];
    ASSERT_ERRNO(s.fd = open(dev, O_RDWR));
    raw_serial_config(s.fd);
    test(&s);
    return 0;

}

