#ifndef MOD_MINMAX_VIEWMODEL
#define MOD_MINMAX_VIEWMODEL

/* Viewmodel for minmax wave viewer using tag_u32 protocol.

   This is originally written for a remote display: a DOM/SVG based
   viewer written in JavaScript, communicating via LEB128 encoded
   TAG_U32 over webscoket.

   This module provides:
   - Cursor tracking (such that the view doesn't need this state)
   - MinMax (mipmap) data retrieval.

   A note on the MOD_ architecture.  The main idea here is to make
   small, specialized programs, e.g. a program that can take a logic
   stream on stdin, and present a web interface on a TCP socket.
   There is only ever one store, one datatype, so no energy is spent
   in making the code generic or configurable at run time.  Everything
   is specialized at compile time.
*/


/* Example that connects mod_minmax.c to a tag_u32 handler. */

/* The default is the i16 minmax module.  User can override.
   Note that we are oblivious to the base type. */
#ifndef MOD_MINMAX
#include "mod_minmax_i16.c"
// #include "mod_minmax_8x1.c"
#endif

#ifndef MOD_MINMAX_FILE
#define MOD_MINMAX_FILE "mod_minmax.c"
#endif

#include "tag_u32.h"

#include "os_thread.h"

/* Map this to nop if mutual exclusion is not necesary.
   This should be defined explicitly to avoid silently dropping locking! */
#ifndef MINMAX_EXCLUSIVE
#error need MINMAX_EXCLUSIVE
#endif

/* The name of the map refers to the handler function. */
#define DEF_MAP DEF_TAG_U32_CONST_MAP_HANDLE

struct minmax_map    map;
struct minmax_cursor cursor;

int zoom(struct tag_u32 *req) {
    TAG_U32_UNPACK(req, 0, m, win_w, win_x, level_inc) {
        ASSERT(m->win_w < 10000); // bug guard
        ASSERT(m->win_x < m->win_w);
        struct minmax_minmax buf[m->win_w];
        MINMAX_EXCLUSIVE() {
            int16_t new_level = minmax_cursor_zoom(
                &cursor, &map,
                buf, m->win_w, m->win_x, m->level_inc);
            (void)new_level;
        }
        MINMAX_LOG("new_level = %d\n", new_level);
        send_reply_tag_u32_status(req, 0, (const uint8_t*)buf, sizeof(buf));
        return 0;
    }
    return -1;
}

DEF_MAP(
    map_cmd,
    {"zoom", "cmd", zoom, 3}
    )

DEF_MAP(
    map_root,
    {"window", "map", map_cmd},
    )

/* FIXME: Mutual exclusion access is not implemented in this test.  Do
 * not use two websockets at the same time. */


/* Protocol handler entry point. */
int handle_tag_u32(struct tag_u32 *req) {
    int rv = map_root(req);
    if (rv) {
        /* Always send a reply when there is a from address. */
        LOG("map_root() returned %d\n", rv);
        send_reply_tag_u32_status_cstring(req, 1, "bad_ref");
    }
    return 0;
}



#endif
