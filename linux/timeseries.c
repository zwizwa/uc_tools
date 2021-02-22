/* Timeseries database.

   Manages a collection of (large) time series files and provides
   indexing and viewing operations.

   RPC interface is exposed through tag_u32 on stdio.

*/

#define MMAP_FILE_LOG LOG

#include "mod_minmax.c"
#include "mod_tag_u32_stream.c"
#include "mod_send_tag_u32.c"

/* The name of the map refers to the handler function. */
#define DEF_MAP DEF_TAG_U32_CONST_MAP_HANDLE

int op1(struct tag_u32 *req) {
    TAG_U32_UNPACK(req, 0, m, win_w, win_x, level_inc) {
        ASSERT(m->win_w < 10000); // arbitrary
        ASSERT(m->win_x < m->win_w);
        struct minmax buf[m->win_w];
        int16_t new_level =
            snapshot_minmax(buf, m->win_w, m->win_x, m->level_inc);
        LOG("new_level = %d\n", new_level);
        send_reply_tag_u32_status(req, 0, (const uint8_t*)buf, sizeof(buf));
        return 0;
    }
    return -1;
}

DEF_MAP(
    map_cmd,
    {"window", "cmd", op1, 3}
    )

DEF_MAP(
    map_root,
    {"cmd", "map", map_cmd},
    )

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

int main(void) {
    generate_minmax(
        //"/tmp/test.raw", // large
        "/tmp/test.bin", // small
        "/tmp/minmax");
    return tag_u32_loop();
}
