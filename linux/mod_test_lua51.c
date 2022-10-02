/* Main idea of this module is to provide access to C application and
   test code from Lua.  This is structurd as a mod_ to make it easier
   to include into a monolith. */

#include "lua_tools.h"

#include "pbuf.h"
#define pbuf_a_T "rdm.pbuf_a"

/* Alternative wrapper for pbufs not using pool. */
struct pbuf_a {
    struct pbuf p;
    uint8_t buf[512+4];
};
void pbuf_a_init(struct pbuf_a *a) {
    pbuf_init(&a->p, a->buf, sizeof(a->buf));
}
#define NS(name) CONCAT(pbuf_a,name)
#include "ns_lua_struct.h"
#undef NS

