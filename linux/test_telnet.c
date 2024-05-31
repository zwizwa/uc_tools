// telnet server on stdout
// test_telnet.sh starts with socat

#include "mod_telnet.c"
#include "macros.h"
void telnet_write_output(struct telnet *, const uint8_t *bytes, uintptr_t len) {
    fwrite(bytes, 1, len, stdout);
    fflush(stdout);
}

// handler just prints a representation of the event
void telnet_event(struct telnet *t, uintptr_t event) {
    uint8_t byte = event & 0xFF;
    event &= ~0xff;

    switch(event) {
    case TELNET_EVENT_INTERRUPT:
        LOG("<INTERRUPT>\n");
        break;
    case TELNET_EVENT_CONTROL:
        LOG("<CONTROL:%d>\n", byte);
        break;
    case TELNET_EVENT_ESCAPE:
        LOG("<ESC:");
        for(uint32_t i=0; i<t->nb_esc; i++) {
            LOG("%c", t->esc[i]);
        }
        LOG(">\n");
        break;
    case TELNET_EVENT_LINE:
        LOG("<LINE:");
        for(uint32_t i=0; i<t->nb_char; i++) {
            LOG("%c", t->line[i]);
        }
        LOG(">\n");
        break;
    case TELNET_EVENT_FLUSH:
        break;
    }
}
int main(int argc, char **argv) {
    struct telnet s;
    telnet_init(&s, telnet_write_output, telnet_event);
    for(;;) {
        uint8_t byte = getchar();
        telnet_write_input(&s, &byte, 1);
    }
    return 0;
}
