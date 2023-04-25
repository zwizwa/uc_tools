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

#include "gdb/rsp_packet.c"
#include "gdb/gdbstub.c"
const char gdbstub_memory_map[] = GDBSTUB_MEMORY_MAP_STM32F103CB;
struct gdbstub_config _config;



/* The gdbstub.h was originally for use on a microcontroller.  The
   memory read/write/erase commands assume a global instance.  Here we
   use objects to be able to use multiple stubs if needed. No time to
   refactor the gdbstub.h code atm, so this context variable is set
   before accessing the stub, to be used by memory commands. */
struct stub_ud;
struct stub_ud *stub_ud_ctx;


/* Data structure wrapper. */
#define stub_T  "gdbstub.stub"
struct stub {
    struct packet req; uint8_t req_buf[GDBSTUB_PACKET_BUFFER_SIZE];
    struct packet rpl; uint8_t rpl_buf[GDBSTUB_PACKET_BUFFER_SIZE];
    struct gdbstub_ctrl ctrl;
    struct gdbstub stub;
    int callbacks_index; // Lua stack index of callback table
};
#define STUB_PKT_INIT(s,pkt) do {                      \
        (s)->stub.pkt = &(s)->pkt;                     \
        (s)->pkt.buf = (s)->pkt##_buf;                 \
        (s)->pkt.size = sizeof((s)->pkt##_buf);        \
    } while(0)
static void stub_init(struct stub *s) {
    ASSERT(s);
    memset(s,0,sizeof(*s));
    STUB_PKT_INIT(s,req);
    STUB_PKT_INIT(s,rpl);
    s->stub.ctrl = &s->ctrl;
    s->stub.commands = gdbstub_default_commands;
    uint32_t reg[GDBSTUB_NB_REGS] = GDBSTUB_REG_INIT;
    memcpy(s->stub.reg, reg, sizeof(reg));
}
#define NS(name) CONCAT(stub,name)
#include "ns_lua_struct.h"
#undef NS

/* Stack read + type check */
static inline const lua_Number number_L(lua_State *L, int index) {
    ASSERT(lua_isnumber(L, index));
    lua_Number n = lua_tonumber(L, index);
    return n;
}
static const char *string_L(lua_State *L, int index, size_t *len) {
    ASSERT(lua_isstring(L, index));
    return lua_tolstring(L, index, len);
}

/* Protocol read/write */
// FIXME: Can probably be removed in favor of putchar and interpret.
static int stub_write_cmd(lua_State *L) {
    struct stub_ud *ud = stub_L(L, -3);
    ud->base.callbacks_index = -2;
    size_t len = 0;
    const char *str = string_L(L, -1, &len);
    stub_ud_ctx = ud;
    //LOG("stub gets %d\n", len);
    gdbstub_write(&ud->base.stub, (const void*)str, len);
    ud->base.callbacks_index = 0; // invalid
    return 0;
}
static int stub_interpret_cmd(lua_State *L) {
    struct stub_ud *ud = stub_L(L, -2);
    ud->base.callbacks_index = -1;
    stub_ud_ctx = ud;
    //LOG("stub gets %d\n", len);
    gdbstub_interpret(&ud->base.stub);
    return 0;
}
static int stub_read_cmd(lua_State *L) {
    struct stub_ud *ud = stub_L(L, -2);
    ud->base.callbacks_index = -1;
    uint32_t n_stub = gdbstub_read_ready(&ud->base.stub);
    //LOG("stub has %d\n", n_stub);
    if (!n_stub) { return 0; } // nil means no data
    uint8_t buf[n_stub];
    stub_ud_ctx = ud;
    gdbstub_read(&ud->base.stub, buf, n_stub);
    lua_pushlstring(L, (const char*)buf, n_stub);
    ud->base.callbacks_index = 0; // invalid
    return 1;
}


/* The packet decoder is exposed so we can handle some commands in
   Lua, e.g. to work around "can't yield from C". */
static int stub_putchar_cmd(lua_State *L) {
    struct stub_ud *ud = stub_L(L, -2);
    char c = number_L(L, -1);
    struct packet *req = ud->base.stub.req;
    int rv = rsp_decode_putchar(req, c);
    lua_pushnumber(L, rv);
    if (rv == E_BUSY) return 1;
    lua_pushlstring(L, (const char*)req->buf, req->wr);
    return 2;
}






/* Called by stub.  We bind it to gdbstub_ub_ctx in the context of a
   Lua call.  All _cmd functions have the memory callbacks on position
   2 on the stack.  Note that the Lua function called cannot yield a
   coroutine from a C call (e.g. fetching a packet from a bus).  In
   order to work around this, some of the lower level functions are
   exposed, so the coroutine call can remain in Lua. */

/* lua_call(ud->L, <nb_args>, <nb_rv> */

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
    struct stub_ud *ud = stub_ud_ctx;
    lua_State *L = ud->L;
    lua_getfield(L, ud->base.callbacks_index, "read");
    lua_pushnumber(L, addr);
    lua_call(L, 1 /* nb arg */, 1 /* nb rv */);
    lua_Number byte = number_L(L, -1);
    lua_pop(L, 1); /* remove so stack doesn't keep growing on next call */
    return byte;
}

int luaopen_gdbstub_lua51 (lua_State *L) {
    new_lua_metatable(L, stub_T, stub_gc);
    lua_newtable(L);
    DEF_CFUN(stub_new);
    DEF_CFUN(stub_read);
    DEF_CFUN(stub_write);
    DEF_CFUN(stub_putchar);
    DEF_CFUN(stub_interpret);
    return 1;
}
