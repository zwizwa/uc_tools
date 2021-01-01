#include "mod_linux_host.c"
void loop() {
}
void setup() {
}
int handle_tag_u32(const struct tag_u32 *s) {
    // echo
    send_tag_u32(s);
    return 0;
}
