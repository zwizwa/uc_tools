#ifndef MOD_PLUGIN
#define MOD_PLUGIN

/* This contains all glue code for plugins.  It will be linked against
   host.x8.f103.elf and can use all symbols present there. */
#include "plugin_api.h"
#include "infof.h"


/* Header at start of .bin file */
void plugin_start(void);
void plugin_stop(void);
void send_reply_tag_u32(const struct tag_u32 *f, const struct tag_u32 *s);

struct plugin_service plugin PLUGIN_HEADER_SECTION = {
    .version = PLUGIN_API_VERSION,
//  .io = { .read = plugin_read, .write = plugin_write },
    .start = plugin_start,
    .stop  = plugin_stop,
    .load_addr = &plugin,
    .endx_addr = &_eplugin,
#ifdef HANDLE_TAG_U32
    .tag_u32_handle = HANDLE_TAG_U32,
    .tag_u32_reply = send_reply_tag_u32, // defined by host
#endif
};



#endif
