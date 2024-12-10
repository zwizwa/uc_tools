/* Wrapper main() for mod_tether_3if.c code */


#include "mod_tether_3if.c"

void tether_handle_async(struct tether *s) {
}


int main(int argc, char **argv) {

    const char *log_tag = getenv("TETHER_BL_TAG");
    if (log_tag) { tether_3if_tag = log_tag; }

    // Need at least device and one command.
    ASSERT(argc >= 3);
    const char *dev = argv[1];

    struct tether s;
    if (dev[0] == '/') {
        /* E.g. /dev/ttyACM0 */
        tether_open_tty(&s, dev);
    }
    else {
        /* IP address.  Change this to ip:port later. */
        tether_open_tcp(&s, dev, 12345);
    }

    const char *verbose = getenv("TETHER_BL_VERBOSE");

    s.verbose = verbose ? atoi(verbose) : 0;
    s.progress = isatty(0);
    s.nb_words = argc-2;
    s.word = argv+2;

    while(s.nb_words != 0) {
        int rv = tether_interpret(&s);
        if (rv != 0) {
            ERROR("rv = %d\n", rv);
        }
    }
}
