#include "mod_linux_host.c"
void loop() {
}
void setup() {
}
int handle_tag_u32(
    void *context,
    const uint32_t *arg,  uint32_t nb_args,
    const uint8_t *bytes, uint32_t nb_bytes) {

    send_tag_u32(context, arg, nb_args, bytes, nb_bytes);

    return 0;
}
