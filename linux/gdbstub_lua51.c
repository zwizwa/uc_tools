/* The GDB stub proxy as a Lua module.
   Derived from gdbstub_proxy.c

   We don't handle the stdio or TTY or TCP end where GDB will
   connection.  That will need to be provided at the Lua end,
   e.g. using libuv.  We only provide read/write to the stub state
   machine.

   The end goal here is to integrate this into an existing Lua app
   that has i/o connection to a number of devices.  I want to reuse
   that to expose a monitor for each of the devices, so I can play
   with live code load to SRAM while the thing is running its
   application.

*/

#include "macros.h"
#include <stdint.h>

#include "gdbstub.h"
#include "lua_tools.h"


/* The gdbstub.h was originally for use on a microcontroller.  The
   memory read/write/erase commands assume a global instance.  Here we
   use objects to be able to use multiple stubs if needed. No time to
   refactor the gdbstub.h code atm, so this context variable is set
   before accessing the stub, to be used by memory commands. */
struct stub_ud;
struct stub_ud *stub_ud_ctx;



#define stub_T  "gdbstub.stub"

struct stub {
    struct packet req; uint8_t req_buf[GDBSTUB_PACKET_BUFFER_SIZE];
    struct packet rpl; uint8_t rpl_buf[GDBSTUB_PACKET_BUFFER_SIZE];
    struct gdbstub_ctrl ctrl;
    struct gdbstub stub;
};

static void stub_init(struct stub *s) {
    ASSERT(s);
    memset(s,0,sizeof(*s));

    s->stub.ctrl     = &s->ctrl;
    s->stub.req      = &s->req;
    s->stub.rpl      = &s->rpl;
    s->stub.commands = gdbstub_default_commands;

    uint32_t reg[GDBSTUB_NB_REGS] = GDBSTUB_REG_INIT;
    memcpy(s->stub.req, reg, sizeof(reg));

}

#if 0
static struct stub *push_gdbstub(lua_State *L, short int tcp_port) {
    struct stub *ud = lua_newuserdata(L, sizeof(*ud));
    ASSERT(ud);
    memset(ud,0,sizeof(*ud));

    ud->tcp_port      = tcp_port;

    ud->stub.ctrl     = &ud->ctrl;
    ud->stub.req      = &ud->req;
    ud->stub.rpl      = &ud->rpl;
    ud->stub.commands = gdbstub_default_commands;

    uint32_t reg[GDBSTUB_NB_REGS] = GDBSTUB_REG_INIT;
    memcpy(ud->stub.req, reg, sizeof(reg));

    luaL_getmetatable(L, gdbstub_T);
    lua_setmetatable(L, -2);
    return ud;
}
static int gdbstub_new_cmd(lua_State *L) {
    short int tcp_port = L_number(L, 1);
    struct stub *ud = push_gdbstub(L, tcp_port);
    (void)ud;
    return 1;
}
static struct stub *gdbstub_L(lua_State *L, int index) {
    ASSERT(luaL_checkudata(L, index, gdbstub_T));
    struct stub *ud = lua_touserdata(L, index);
    return ud;
}
static int gdbstub_gc(lua_State *L) {
    struct stub *ud = gdbstub_L(L, -1);
    (void)ud; // Just a stub
    return 0;
}
#endif


#define NS(name) CONCAT(stub,name)
#include "ns_lua_struct.h"
#undef NS


void *buf;
int nb;

static const char *string_L(lua_State *L, int index, size_t *len) {
    ASSERT(lua_isstring(L, index));
    return lua_tolstring(L, index, len);
}
static int stub_write_cmd(lua_State *L) {
    struct stub_ud *ud = stub_L(L, 1);
    size_t len = 0;
    const char *str = string_L(L, 2, &len);
    stub_ud_ctx = ud;
    gdbstub_write(&ud->base.stub, (const void*)str, len);
    stub_ud_ctx = NULL;
    return 0;
}
static int stub_read_cmd(lua_State *L) {
    struct stub_ud *ud = stub_L(L, -1);
    uint32_t n_stub = gdbstub_read_ready(&ud->base.stub);
    if (!n_stub) { return 0; } // nil means no data
    uint8_t buf[n_stub];
    stub_ud_ctx = ud;
    gdbstub_read(&ud->base.stub, buf, n_stub);
    stub_ud_ctx = NULL;
    lua_pushlstring(L, (const char*)buf, n_stub);
    return 1;
}


static int name_cmd(lua_State *L) {
    const char *str = "gdbstub_lua51";
    lua_pushstring(L, str);
    return 1;
}


/* Wrappers that bind gdbstub_ub_ctx. */
int32_t flash_erase(uint32_t addr, uint32_t size) {
    return 0;
}
int32_t flash_write(uint32_t addr, const uint8_t *b_buf, uint32_t len) {
    return 0;
}
int32_t mem_write(uint32_t addr, uint8_t val) {
    return E_OK;
}
int32_t mem_write32(uint32_t addr, uint32_t val) {
    return E_OK;
}
uint8_t mem_read(uint32_t addr) {
    return 0x55;
}

int luaopen_gdbstub_lua51 (lua_State *L) {
    new_lua_metatable(L, stub_T, stub_gc);

    lua_newtable(L);
    DEF_CFUN(name);
    DEF_CFUN(stub_new);
    DEF_CFUN(stub_read);
    DEF_CFUN(stub_write);
#undef CMD
    return 1;
}
