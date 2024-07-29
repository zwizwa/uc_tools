/* Configuration for bp6 board.
   Ad-hoc debugging board.

   The idea is to gather generic bits in mod_lab.c to have it work as
   an application framework, and define the app instance here.

*/

#define PRODUCT "bp6"
#include "mod_lab.c"
#include "mod_console.c"
#include "mod_map_forth.c"

//////////////////////////////////////////////////////////////////////

#if 0
void mark_stack(void) {
#ifndef EMU
    /* FIXME: This is very much undefined behavior and will need to be
       written in assembler to be able to guarantee anything.  We just
       add a guard offset in case there is some stack allocation going
       on for the loop.  The stack usage of the application will be
       more than that. */
    uint8_t *bottom = 0;
    __asm__ volatile ("mov %0, sp\n\t" : "=r" ( bottom ) );
    bottom -= 100;
    for(uint8_t *p=_service.stack_lo; p<bottom; p++) *p = 0x55;
#if 0
    uintptr_t nb = bottom - (uint8_t*)_service.stack_lo;
    infof("stack %x %d\n", _service.stack_lo, nb);
#endif
#endif
}
void check_stack(void) {
#ifndef EMU
    uint8_t *bottom = 0;
    __asm__ volatile ("mov %0, sp\n\t" : "=r" ( bottom ) );
    uint32_t unused = 0;
    for(uint8_t *p=_service.stack_lo; p<bottom; p++) {
        if (*p != 0x55) {
            unused = p - (uint8_t*)_service.stack_lo;
            break;
        }
    }
    infof("unused = %d\n", unused);
#endif
}
COMMAND_REGISTER(mark_stack);
COMMAND_REGISTER(check_stack);
#endif


//////////////////////////////////////////////////////////////////////

void app_poll(void) {
}

instance_status_t app_init(instance_init_t *ctx) {
    infof("product: %s\n", PRODUCT);
    INSTANCE_NEED(ctx, &console);
    _service.add(app_poll);
    return 0;
}
DEF_INSTANCE(app);

int map_root(struct tag_u32 *req) {
    const struct tag_u32_entry map[] = {
        {"forth", "map", map_forth},
    };
    return HANDLE_TAG_U32_MAP(req, map);
}
int handle_tag_u32(struct tag_u32 *req) {
    int rv = map_root(req);
    if (rv) {
        infof("handle_tag_u32 returned %d\n", rv);
        /* Always send a reply when there is a from address. */
        send_reply_tag_u32_status_cstring(req, 1, "bad_ref");
    }
    return 0;
}

