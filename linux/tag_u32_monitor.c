/* A monitor for TAG_U32 packets. */
#include "mod_linux_host.c"
void setup(void) {}
void loop(void) {}
int handle_tag_u32(
    void *context,
    const uint32_t *arg,  uint32_t nb_args,
    const uint8_t *bytes, uint32_t nb_bytes) {

    for(int i=0; i<nb_args;  i++) { printf(" %08x", arg[i]);   }
    for(int i=0; i<nb_bytes; i++) { printf(" %02x", bytes[i]); }
    printf("\n");
    return 0;
}
