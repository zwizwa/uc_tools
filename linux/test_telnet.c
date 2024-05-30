#include "mod_telnet.c"
#include "macros.h"
// see test_telnet.sh
void telnet_write_output(struct telnet *, const uint8_t *bytes, uintptr_t len) {
    fwrite(bytes, 1, len, stdout);
    fflush(stdout);
}
int main(int argc, char **argv) {
    struct telnet s;
    telnet_init(&s, telnet_write_output);
    for(;;) {
        uint8_t byte = getchar();
        telnet_write_input(&s, &byte, 1);
    }
    return 0;
}
