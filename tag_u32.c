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

/* Note that this is not easy to debug, so print error messages for
   all cases that do not send a reply.  Currently there is no generic
   error mechanism to send errors back using RPC.  Maybe that should
   be added?  E.g at least make an ok/error distinction. */

int handle_tag_u32_map(struct tag_u32 *r, const struct tag_u32_entry *map, uint32_t nb_entries) {
    if (r->nb_args < 1) {
        LOG("handle_tag_u32_map: missing arg\n");
        return -1;
    }
    uint32_t i = r->args[0];
    /* Delegate. */
    if (i < nb_entries) {
        tag_u32_enter(r);
        int rv = -1;
        if ((map[i].nb_args < 0) || (r->nb_args >= map[i].nb_args)) {
            rv = map[i].handle(r);
        }
        else {
            LOG("handle_tag_u32_map: nb_args = %d, expected %d\n",
                r->nb_args, map[i].nb_args);
        }
        tag_u32_leave(r);
        return rv;
    }
    /* Serve metadata. */
    TAG_U32_MATCH(r, TAG_U32_CTRL, m, cmd, id) {
        const char *str = NULL;
        if (m->id < nb_entries) {
            switch(m->cmd) {
            case TAG_U32_CTRL_ID_NAME: str = map[m->id].name; break;
            case TAG_U32_CTRL_ID_TYPE: str = map[m->id].type; break;
            }
        }
        if (str) {
            send_reply_tag_u32_status_cstring(r, 0, str);
        }
        else {
            SEND_REPLY_TAG_U32(r, -1);
        }
        return 0;
    }
    TAG_U32_MATCH(r, TAG_U32_CTRL, m, cmd) {
        if (m->cmd == TAG_U32_CTRL_NAME_ID &&
            r->nb_args == 2 &&
            r->nb_bytes > 0) {
            for (uint32_t i=0; i<nb_entries; i++) {
                if ((strlen(map[i].name) == r->nb_bytes) &&
                    (!memcmp(map[i].name, r->bytes, r->nb_bytes))) {
                    SEND_REPLY_TAG_U32(r, 0, i);
                    return 0;
                }
            }
            SEND_REPLY_TAG_U32(r, -1);
            return 0;
        }
    }
    LOG("handle_tag_u32_map: bad command %d\n", r->args[0]);
    return -1;
}
