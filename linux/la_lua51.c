/* A wrapper around the logic analyzer server. See also la_ws.c

   The way this is hooked up is very pragmatic: the point for this is
   to be included in a test system that is written in Lua, and I want
   to make sure that not a whole lot of time is lost in marshalling
   data back and forth, so principle one:

   1. Interface between analyzer and Lua is as a Lua C module, such
      that we can just push lua tables and strings from C.

   2. Anything else is just bolted on top, such as the web interface.

   3. Use the mod_ system

*/


#include "macros.h"
#include "os_linux.h"
#include "os_thread.h"
#include "assert_execvp.h"
#include <stdint.h>

#include "lua_tools.h"

os_mutex_t minmax_lock;
#define MINMAX_EXCLUSIVE() \
    WITH_PRE_POST(&minmax_lock, os_mutex_lock, os_mutex_unlock)

#include "mod_minmax_8x1.c"
#include "mod_minmax_viewmodel.c"
#include "mod_webserver.c"
#include "mod_websocket_leb128s.c"

struct uart_out;
static void frame_out(struct uart_out *s);

#include "mod_la_dmx.c"


/* One slice is currently 1MB, same as Saleae C++ code buffer size.
   Samplerate is currently 2Msps
   So once slice is half a second.
   30 seconds is probably enough. */
uintptr_t nb_slices = 60;

static int cmd_name(lua_State *L) {
    const char *str = "la_lua51";
    lua_pushstring(L, str);
    return 1;
}

/* Trigger mechanism.  Currently only edge triggering on all
   simultaneous busses is supported.  User sets the idle levels and
   the timestamp of the first change is recorded. */

uint8_t triggered = 0;
uint8_t trig_last = 0xFF;
uintptr_t trig_loc = 0;
int cmd_arm(lua_State *L) {
    double d_trig = lua_tonumber(L, -1);
    trig_last = d_trig;
    triggered = 0;
    lua_pop(L,1);
    return 0;
}
int cmd_get_trigger(lua_State *L) {
    double d_trig = trig_loc;
    lua_pushnumber(L, d_trig);
    return 1;
}

/* Logic analyzer */
#define FOR_PORTS(m) m(0) m(1) m(2)
#define PUSH(n) la_uart_push(&s_##n, &e);
FOR_PORTS(DEF_PORT)
struct la_event e = {};

static void frame_out(struct uart_out *s) {
    /* Called when frame is completed.
       Just pushed to a stack. */
    // FIXME: Also push out to the websocket?
    frame_push(s);
}
void analyzer(const uint8_t *data, uintptr_t slice) {
    if (triggered) return;
    for(uintptr_t i=0; i<MINMAX_SLICE_SIZE; i++) {
        e.value = data[i];
        FOR_PORTS(PUSH);
        e.time++;
        if (!triggered && (data[i] != trig_last)) {
            triggered = 1;
            trig_loc = e.time;
            LOG("trigger %d\n", trig_loc);
        }
    }
}



int in_fd = -1;
int out_fd = -1;
OS_THREAD_STACK(la_thread, 1024);
OS_THREAD_MAIN(la_loop, ctx) {
    uint8_t *circ_buf = map.level[0].buf;
    uintptr_t slice_nb= 0;
    for(;;) {
        LOG("\r%03d", slice_nb);
        uint8_t *buf = circ_buf + MINMAX_SLICE_SIZE * slice_nb;
        /* Note that we are writing straight into the buffer without
           locking.  If gui requests level 0 data it might get new or
           old. We can't just put the read in a critical section
           because then there is no time slot left for the view.
           However, only the data changes, and none of the indexing,
           so the worst that can happen is display glitches.  Deal
           with it later. */
        assert_read_fixed(in_fd, buf, MINMAX_SLICE_SIZE);
        minmax_update_slice(&map, slice_nb);
        analyzer(buf, slice_nb);
        slice_nb = (slice_nb + 1) % nb_slices;
    }
    return 0;
}

/* FIXME: Stub to get current time stamp, but this actually doesn't
   work, since we don't have any (high resolution) time base other
   than the signal itself. */
static int cmd_mark(lua_State *L) {
    return 0;
}
static int cmd_start(lua_State *L) {
    const char *program = lua_tostring(L, -1);
    minmax_open_buf(&map, "/tmp/la_lua51.bin", 8, nb_slices);
    LOG("starting %s\n", program);
    const char *argv[] = {program, NULL};
    assert_fork_execvp(&in_fd, &out_fd, argv, NULL);
    lua_pop(L,1);
    OS_THREAD_START(la_thread, la_loop, NULL);
    return 0;
}

static int cmd_status(lua_State *L) {
    sleep(1);
    const char *str = "ok";
    lua_pushstring(L, str);
    return 1;
}

OS_THREAD_STACK(http_thread, 1024);
uint16_t tcp_port = 0;
OS_THREAD_MAIN(http_loop, ctx) {
    webserver_loop(tcp_port);
    return 0;
}
static int cmd_start_http(lua_State *L) {
    tcp_port = lua_tonumber(L, -1);
    // FIXME: cd to directory?  Or just use cwd?
    // Maybe best to not let http server rely on cwd instead...
    OS_THREAD_START(http_thread, http_loop, NULL);
    lua_pop(L,1);
    return 0;
}

os_error_t dummy_read(struct blocking_io *io, uint8_t *buf, uintptr_t len) {
    return NULL;
}
os_error_t dummy_write(struct blocking_io *io, const uint8_t *buf, uintptr_t len) {
    //LOG("dummy_write: %d\n", len);
    for (uintptr_t i=0; i<len; i++) LOG(" %02x", buf[i]);
    return NULL;
}
static struct blocking_io dummy_io = {
    .read = dummy_read,
    .write = dummy_write,
};
static int cmd_send(lua_State *L) {
    struct blocking_io *io = NULL;
    if (!io) {
        io = &dummy_io;
    }
    const char *payload = assert_lua_tostring(L, -1);
    uint32_t args[] = {1,2,3};
    struct tag_u32 msg = {
        .reply = (void*)dummy_write, .reply_ctx = io,
        .args = args, .nb_args = ARRAY_SIZE(args),
        .bytes = (const void*)payload, .nb_bytes = strlen(payload),
    };
    //(void)&msg;
    send_tag_u32(&msg);
    LOG("\n");
    lua_pop(L,1);
    return 0;
}

int luaopen_la_lua51 (lua_State *L) {
    lua_newtable(L);
#define CMD(_name) { \
    lua_pushcfunction (L, cmd_##_name); \
    lua_setfield (L, -2, #_name); \
    }
    CMD(name);
    CMD(start);
    CMD(mark);
    CMD(status);
    CMD(arm);
    CMD(get_trigger);
    CMD(start_http);
    CMD(send);
#undef CMD
    return 1;
}
