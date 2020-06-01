// Don't bother with linking. Just include. Also makes it easier to
// redefine things, and stub some things out in the firmware based on
// EMU macro.
#define EMU
#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#define LOG(...) fprintf(stderr, __VA_ARGS__)
#define printk LOG
#define BT_UUID_INIT_16(_uuid) { .val = _uuid }
struct bt_uuid_16 { uint16_t val; };
#include "spec_impl.h"
#include "appdata.c"

#if 1
DEF_VAR_GET_SET(char11)
DEF_VAR_GET_SET(char12)
DEF_VAR_GET_SET(char13)
DEF_VAR_GET_SET(char21)
#endif

FOR_SERVICES(DEF_SERVICE)
const struct appdata_service *services[] = { FOR_SERVICES(REF_SERVICE) NULL };


int main(int argc, char **argv) {
    LOG("test.c\n");
    printk_services(services);
}
