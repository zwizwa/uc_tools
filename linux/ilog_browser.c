#include "mod_ilog_browser.c"

int main(int argc, char **argv) {
    if (argc != 2) {
        LOG("usage: %s <ilog>\n", argv[0]);
        exit(1);
    }
    ib_loop(argv[1]);
}
