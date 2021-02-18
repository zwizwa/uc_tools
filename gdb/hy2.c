
#define PRODUCT "hy2"

#include "mod_lab.c"

instance_status_t app_init(instance_init_t *i) {
    INSTANCE_NEED(i, &console);
    return 0;
}
DEF_INSTANCE(app);
#include "mod_map_forth.c"

/* root map */
int map_root(struct tag_u32 *req) {
    const struct tag_u32_entry map[] = {
        {"forth",    "map", map_forth},
    };
    return HANDLE_TAG_U32_MAP(req, map);
}

/* Protocol handler entry point. */
int handle_tag_u32(struct tag_u32 *req) {
    return map_root(req);
}
