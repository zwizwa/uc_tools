#include "tag_u32.h"
#include "infof.h"

/* This allcates temp buffers for the decoded tags. It is assumed all
   temp buffers fit on the stack. */
int tag_u32_dispatch(tag_u32_handle_fn handler,
                     tag_u32_reply_fn reply,
                     void *context,
                     const uint8_t *buf, uint32_t nb_buf) {
    if (nb_buf < 4) return TAG_U32_ERROR_SIZE;

    // This format is now also used for TAG_COMMAND, so don't check
    // uint32_t tag = read_be(buf, 2);
    // if (tag != TAG_U32) return TAG_U32_ERROR_TAG;

    /* Check if size parameters make sense. */
    uint32_t nb_f = buf[2];
    uint32_t nb_a = buf[3];
    uint32_t offset_b = 2 + 2 + 4 * (nb_f + nb_a);
    if (nb_buf < offset_b) return TAG_U32_ERROR_SIZE;

    /* Everything after the tag vectors is opaque payload. */
    uint32_t nb_b = nb_buf - offset_b;

    /* Unpack "from" and "arg" tags, fill in index struct and delegate. */

    const uint8_t *buf_f = buf   + 2 + 2;
    const uint8_t *buf_a = buf_f + 4 * nb_f;

    uint32_t f[nb_f];  read_be_u32_array(f, buf_f, nb_f);
    uint32_t a[nb_a];  read_be_u32_array(a, buf_a, nb_a);

    struct tag_u32 s = {
        .context = context,
        .reply = reply,
        .from = f, .nb_from = nb_f,
        .args = a, .nb_args = nb_a,
        .bytes = buf + offset_b, .nb_bytes = nb_b
    };
    return handler(&s);
}


/* Generic dispatch of nodes and metadata based on metadata table. */
int handle_tag_u32_map(struct tag_u32 *r, const struct tag_u32_entry *map, uint32_t nb_entries) {
    if (r->nb_args < 1) return -1;
    uint32_t i = r->args[0];
    /* Delegate. */
    if (i < nb_entries) {
        tag_u32_enter(r);
        int rv = -1;
        if ((map[i].nb_args < 0) || (r->nb_args >= map[i].nb_args)) {
            rv = map[i].handle(r);
        }
        tag_u32_leave(r);
        return rv;
    }
    /* Serve metadata. */
    TAG_U32_MATCH(r, TAG_U32_CTRL, m, cmd, id) {
        /* Empty string means not defined. */
        const char *str = "";
        if (m->id < nb_entries) {
            switch(m->cmd) {
            case TAG_U32_CTRL_ID_NAME: str = map[m->id].name; break;
            case TAG_U32_CTRL_ID_TYPE: str = map[m->id].type; break;
            }
        }
        SEND_REPLY_TAG_U32_CSTRING(r, str);
        return 0;
    }
    return -1;
}
