/* See other http_*.c examples. */


#include "mod_webserver.c"
#include "mod_websocket_leb128s.c"

void send_reply_tag_u32_status_cstring(
    const struct tag_u32 *req, uint32_t status, const char *string) {
    LOG("status %u %s\n", status, string);
}

/* Protocol handler entry point.
   Just a stub. See e.g. mod_minmax_viewmodel.c */
int handle_tag_u32(struct tag_u32 *req) {
    for (uint32_t i=0; i<req->nb_args; i++) {
        LOG(" %u", req->args[i]);
    }
    LOG("\n");
    send_reply_tag_u32_status_cstring(req, 1, "bad_ref");
    return 0;
}

int main(int argc, char **argv) {
    if (argc != 2) {
        LOG("usage: %s <documentroot>\n", argv[0]);
        return 1;
    }
    ASSERT_ERRNO(chdir(argv[1]));
    uint16_t port = 5678;
    LOG("starting server on port %d\n", port);
    webserver_loop(port);
}
