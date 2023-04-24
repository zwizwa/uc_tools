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
};
#define STUB_BUF_INIT(s,pkt) do {                      \
        (s)->stub.pkt = &(s)->pkt;                     \
        (s)->pkt.buf = (s)->pkt##_buf;                 \
        (s)->pkt.size = sizeof((s)->pkt##_buf);        \
    } while(0)

static void stub_init(struct stub *s) {
    ASSERT(s);
    memset(s,0,sizeof(*s));
    STUB_BUF_INIT(s,req);
    STUB_BUF_INIT(s,rpl);
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
static int stub_write_cmd(lua_State *L) {
    struct stub_ud *ud = stub_L(L, 1);
    size_t len = 0;
    const char *str = string_L(L, 2, &len);
    stub_ud_ctx = ud;
    LOG("stub gets %d\n", len);
    gdbstub_write(&ud->base.stub, (const void*)str, len);
    return 0;
}
static int stub_read_cmd(lua_State *L) {
    struct stub_ud *ud = stub_L(L, -1);
    uint32_t n_stub = gdbstub_read_ready(&ud->base.stub);
    LOG("stub has %d\n", n_stub);
    if (!n_stub) { return 0; } // nil means no data
    uint8_t buf[n_stub];
    stub_ud_ctx = ud;
    gdbstub_read(&ud->base.stub, buf, n_stub);
    lua_pushlstring(L, (const char*)buf, n_stub);
    return 1;
}


/* Called by stub.  We bind it to gdbstub_ub_ctx in the context of a Lua call. */

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
    return 0x55;
}

int luaopen_gdbstub_lua51 (lua_State *L) {
    new_lua_metatable(L, stub_T, stub_gc);

    lua_newtable(L);
    DEF_CFUN(stub_new);
    DEF_CFUN(stub_read);
    DEF_CFUN(stub_write);
#undef CMD
    return 1;
}
