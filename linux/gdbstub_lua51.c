/* Expose gdbstub.c functionality to Lua

   This Lua implementation preserves most of the original gdbstub
   on-target semantics, which was intended to provide access to GDB
   for memory readout and code execution while an application's main
   loop is running.  The main difference with the on-target version is
   that we run the GDB RSP protocol at the host end, and only rely on
   abstract memory read and code execution RPC mechanism on the
   target.  This makes it much easer to integrate the functionality in
   an existing application.  This resembles the 3-instruction Forth
   mechanism as is used in the bootloader.

   See also gdbstub_proxy.c (which uses gdbstub.c to expose a memory
   dump as a gdbstub) and the original gdbstub.c on-target
   implementation in uc_tools.

   The Lua end of this is in gdbstub.lua which implements the TCP
   server where GDB will connect, and overrides some commands to allow
   for coroutine-based RPC.
*/

#include "macros.h"
#include <stdint.h>

#include "gdbstub.h"
#include "lua_tools.h"

#include "gdb/rsp_packet.c"
#include "gdb/gdbstub.c"
const char gdbstub_memory_map[] = GDBSTUB_MEMORY_MAP_STM32F103CB;
struct gdbstub_config _config;



/* Data structure wrapper. */
#define stub_T  "gdbstub.stub"
struct stub {
    struct packet req; uint8_t req_buf[GDBSTUB_PACKET_BUFFER_SIZE];
    struct packet rpl; uint8_t rpl_buf[GDBSTUB_PACKET_BUFFER_SIZE];
    struct gdbstub_ctrl ctrl;
    struct gdbstub stub;
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
    struct stub_ud *ud = stub_L(L, 1);
    size_t len = 0;
    const char *str = string_L(L, 2, &len);
    //LOG("stub gets %d\n", len);
    gdbstub_write(&ud->base.stub, (const void*)str, len);
    return 0;
}
static int stub_interpret_cmd(lua_State *L) {
    struct stub_ud *ud = stub_L(L, 1);
    //LOG("stub gets %d\n", len);
    gdbstub_interpret(&ud->base.stub);
    return 0;
}
static int stub_read_cmd(lua_State *L) {
    struct stub_ud *ud = stub_L(L, 1);
    uint32_t n_stub = gdbstub_read_ready(&ud->base.stub);
    //LOG("stub has %d\n", n_stub);
    if (!n_stub) { return 0; } // nil means no data
    uint8_t buf[n_stub];
    gdbstub_read(&ud->base.stub, buf, n_stub);
    lua_pushlstring(L, (const char*)buf, n_stub);
    return 1;
}


/* The packet decoder is exposed so we can handle some commands in
   Lua, e.g. to work around "can't yield from C". */
static int stub_putchar_cmd(lua_State *L) {
    struct stub_ud *ud = stub_L(L, 1);
    char c = number_L(L, 2);
    struct packet *req = ud->base.stub.req;
    int rv = rsp_decode_putchar(req, c);
    lua_pushnumber(L, rv);
    if (rv == E_BUSY) return 1;
    lua_pushlstring(L, (const char*)req->buf, req->wr);
    return 2;
}
static int rpl_begin_cmd(lua_State *L) {
    struct stub_ud *ud = stub_L(L, 1);
    packet_init(ud->base.stub.rpl);
    rsp_begin(ud->base.stub.rpl);
    return 0;
}
static int rpl_save_cs_cmd(lua_State *L) {
    struct stub_ud *ud = stub_L(L, 1);
    uint8_t byte = number_L(L, 2);
    int32_t rv = packet_save_cs(ud->base.stub.rpl, byte);
    lua_pushnumber(L, rv);
    return 1;
}
static int rpl_save_hex_cs_cmd(lua_State *L) {
    struct stub_ud *ud = stub_L(L, 1);
    uint8_t byte = number_L(L, 2);
    int32_t rv = packet_save_hex_cs(ud->base.stub.rpl, byte);
    lua_pushnumber(L, rv);
    return 1;
}
static int rpl_end_cmd(lua_State *L) {
    struct stub_ud *ud = stub_L(L, 1);
    rsp_end(ud->base.stub.rpl);
    return 0;
}


/* To implement function exection from gdb, reuse most of the
   mechanism from the original on-target gdbstub, but override the "c"
   (continue) command in Lua.  The function below can be used to
   retreive the function arguments from registers r0,r1,r2,r3 */
static int reg_read_cmd(lua_State *L) {
    struct stub_ud *ud = stub_L(L, 1);
    uintptr_t reg_nb = number_L(L, 2);
    ASSERT(reg_nb < GDBSTUB_NB_REGS);
    lua_pushnumber(L, ud->base.stub.reg[reg_nb]);
    return 1;
}
static int stub_return_cmd(lua_State *L) {
    struct stub_ud *ud = stub_L(L, 1);
    uint32_t rv = number_L(L, 2);
    gdbstub_fake_stop_at_breakpoint(&ud->base.stub, rv);
    return 0;
}







/* These are just stubs.  If different behavior is necessary, override
   the packet interpretation in Lua.  Note that this first had a Lua
   callback mechanism, but typically I want to do a coroutine yield,
   which doesn't work across C calls. */
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
    return 0;
}

int luaopen_gdbstub_lua51 (lua_State *L) {
    new_lua_metatable(L, stub_T, stub_gc);
    lua_newtable(L);
    DEF_CFUN(stub_new);
    DEF_CFUN(stub_read);
    DEF_CFUN(stub_write);
    DEF_CFUN(stub_putchar);
    DEF_CFUN(stub_interpret);
    DEF_CFUN(rpl_begin);
    DEF_CFUN(rpl_save_cs);
    DEF_CFUN(rpl_save_hex_cs);
    DEF_CFUN(rpl_end);
    DEF_CFUN(reg_read);
    DEF_CFUN(stub_return);
    return 1;
}
