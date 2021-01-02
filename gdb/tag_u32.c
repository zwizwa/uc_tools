#include "tag_u32.h"
#include "infof.h"

#define REPLY_LOG infof

#define TAG_U32_REPLY_META(r, arr, nb_el,  index, field) \
    if (index < nb_el) { SEND_REPLY_TAG_U32_CSTRING(r, arr[index].field); } else { SEND_REPLY_TAG_U32_CSTRING(r, "") }

/* Generic dispatch of nodes and metadata based on metadata table. */
int handle_tag_u32_map(struct tag_u32 *r, const struct tag_u32_entry *map, uint32_t nb_entries) {
    if (r->nb_args >= 1) {
        uint32_t i = r->args[0];
        if (i < nb_entries) {
            tag_u32_enter(r);
            if ((map[i].nb_args < 0) || (r->nb_args >= map[i].nb_args)) {
                map[i].handle(r);
            }
            tag_u32_leave(r);
        }
    }
    /* Method metadata. */
    TAG_U32_MATCH(r, TAG_U32_CTRL, m, cmd) {
        tag_u32_enter(r);
        TAG_U32_MATCH(r, TAG_U32_CTRL_ID_NAME, m, id) {
            TAG_U32_REPLY_META(r, map, nb_entries, m->id, name);
        }
        TAG_U32_MATCH(r, TAG_U32_CTRL_ID_TYPE, m, id) {
            TAG_U32_REPLY_META(r, map, nb_entries, m->id, type);
        }
        tag_u32_leave(r);
    }
    return -1;
}
