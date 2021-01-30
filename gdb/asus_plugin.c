#include "mod_poll_plugin.c"

uint32_t plugin_timer;
void plugin_poll(void) {
    MS_PERIODIC(plugin_timer, 1000) {
        //infof("tick\n");
    }
}
int crash(struct tag_u32 *s) {
    volatile int dummy = 0;
    for(;;) { (void)dummy; }
}
int echo(struct tag_u32 *req) {
    send_reply_tag_u32_ok_u32(req, req->args[0]);
    return 0;
}
int timer(struct tag_u32 *req) {
    send_reply_tag_u32_ok_u32(req, plugin_timer);
    return 0;
}
int plugin_tag_u32(struct tag_u32 *req) {
    const struct tag_u32_entry map[] = {
        {"crash",  "cmd", 0, crash},
        {"echo",   "cmd", 1, echo},
        {"timer",  "cmd", 0, timer},
    };
    return HANDLE_TAG_U32_MAP(req, map);
}
void plugin_init(void) {
    infof("plugin_init: %s\n", __FILE__);
}
