/* A monitor for TAG_U32 packets. */
#include "mod_linux_host.c"
void setup(void) {}
void loop(void) {}
int handle_tag_u32(const struct tag_u32 *s) {

    for(int i=0; i<s->nb_args;  i++) { printf(" %08x", s->args[i]);  }
    for(int i=0; i<s->nb_bytes; i++) { printf(" %02x", s->bytes[i]); }
    printf("\n");
    return 0;
}
