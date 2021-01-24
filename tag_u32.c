#include "tag_u32.h"
#include "log.h"
//#include "infof.h"

int tag_u32_reply_bad_map_ref(struct tag_u32 *req) {
    send_reply_tag_u32_status_cstring(req, 1, "bad_map_ref");
    return 0;
}


/* This allcates temp buffers for the decoded tags. It is assumed all
   temp buffers fit on the stack. */
int tag_u32_dispatch(tag_u32_handle_fn handler,
                     tag_u32_reply_fn reply,
                     void *context,
                     const uint8_t *buf, uint32_t nb_buf) {

    if (0) {
        LOG("packet: "); for(uint32_t i=0; i<nb_buf; i++) { LOG(" %02x", buf[i]); } LOG("\n");
    }

    if (nb_buf < 4) return TAG_U32_ERROR_SIZE;

    // This format is now also used for TAG_COMMAND, so don't check
    // uint32_t tag = read_be(buf, 2);
    // if (tag != TAG_U32) return TAG_U32_ERROR_TAG;

    /* Check if size parameters make sense. */
    uint32_t nb_f = buf[2];
    uint32_t nb_a = buf[3];
    uint32_t offset_b = 2 + 1 + 1 + 4 * (nb_f + nb_a);
    if (nb_buf < offset_b) return TAG_U32_ERROR_SIZE;

    /* Everything after the tag vectors is opaque payload. */
    uint32_t nb_b = nb_buf - offset_b;

    /* Unpack "from" and "arg" tags, fill in index struct and delegate. */

    const uint8_t *buf_f = buf   + 2 + 2;
    const uint8_t *buf_a = buf_f + 4 * nb_f;

    uint32_t f[nb_f];  read_be_u32_array(f, buf_f, nb_f);
    uint32_t a[nb_a];  read_be_u32_array(a, buf_a, nb_a);

    if (0) {
        LOG("from:"); for(uint32_t i=0;i<nb_f; i++) { LOG(" %08x", f[i]); } LOG("\n");
        LOG("args:"); for(uint32_t i=0;i<nb_a; i++) { LOG(" %08x", a[i]); } LOG("\n");
    }

    struct tag_u32 s = {
        .context = context,
        .reply = reply,
        .from = f, .nb_from = nb_f,
        .args = a, .nb_args = nb_a,
        .bytes = buf + offset_b, .nb_bytes = nb_b
    };
    int rv = handler(&s);
    if (rv && 1)  {
        LOG("tag_u32_dispatch rv=%d, path:", rv);
        for(uint32_t i=0; i<nb_a; i++) {
            LOG(" %d",a[i]);
        }
        LOG("\n");
    }
    return rv;
}



/* The main map handler is implemented with abstract map reference, to
   avoid having to create large arrays for dynamically generated
   maps. */

static inline int tag_u32_do_map_ref(
    map_ref_fn map_ref, uint32_t index,
    struct tag_u32 *r, void *ctx,
    struct tag_u32_entry *entry) {

    /* Modify the path to reflect the path structure of the actual
       directory that this is providing information about, i.e. hiding
       the control command from map_ref. */
    uint32_t cmd     = r->args[0];
    uint32_t nb_args = r->nb_args;
    r->args[0] = index;
    r->nb_args = 1;
    int rv = map_ref(r, ctx, entry);
    r->args[0] = cmd;
    r->nb_args = nb_args;
    return rv;
}


int handle_tag_u32_map_ref_meta(struct tag_u32 *r,
                                map_ref_fn map_ref, void *ctx) {
    struct tag_u32_entry entry = {};

    /* Serve metadata. */
    TAG_U32_MATCH(r, TAG_U32_CTRL, m, cmd, id) {
        const char *str = NULL;
        int rv = tag_u32_do_map_ref(map_ref, m->id, r, ctx, &entry);
        /* !rv means the entry is valid.
           Note that the string might still be NULL, for unnamed nodes. */
        if (!rv) {
            switch(m->cmd) {
            case TAG_U32_CTRL_ID_NAME: str = entry.name; break;
            case TAG_U32_CTRL_ID_TYPE: str = entry.type; break;
            }
        }
        if (!rv) {
            send_reply_tag_u32_status_cstring(r, 0, str);
        }
        else {
            SEND_REPLY_TAG_U32(r, 1);
        }
        return 0;
    }
    TAG_U32_MATCH(r, TAG_U32_CTRL, m, cmd) {
        if (m->cmd == TAG_U32_CTRL_NAME_ID &&
            r->nb_args == 2 &&
            r->nb_bytes > 0) {
            int rv = -1;
            for (uint32_t i=0;
                 !(rv = tag_u32_do_map_ref(
                       map_ref, i, r, ctx, &entry));
                 i++) {
                if ((strlen(entry.name) == r->nb_bytes) &&
                    (!memcmp(entry.name, r->bytes, r->nb_bytes))) {
                    SEND_REPLY_TAG_U32(r, 0, i);
                    return 0;
                }
            }
            SEND_REPLY_TAG_U32(r, 1);
            return 0;
        }
    }
    send_reply_tag_u32_status_cstring(r, 1, "bad_comand");
    return 0;
}


/* This is a wrapper for a concrete array. */
struct tag_u32_map_ref {
    const struct tag_u32_entry *map;
    uint32_t nb_entries;
};
int tag_u32_map_ref(
    struct tag_u32 *r, void *ctx,
    struct tag_u32_entry *entry) {

    struct tag_u32_map_ref *mr = ctx;
    uint32_t index = r->args[0];
    if (index >= mr->nb_entries) return -1;
    *entry = mr->map[index];
    return 0;
}
int handle_tag_u32_map_meta(struct tag_u32 *r,
                            const struct tag_u32_entry *map,
                            uint32_t nb_entries) {
    struct tag_u32_map_ref mr = { .map = map, .nb_entries = nb_entries };
    return handle_tag_u32_map_ref_meta(r, tag_u32_map_ref, &mr);
}

int handle_tag_u32_map(struct tag_u32 *r,
                       const struct tag_u32_entry *map, uint32_t nb_entries) {
    if (r->nb_args < 1) {
        LOG("handle_tag_u32_map: missing arg\n");
        return -1;
    }
    uint32_t i = r->args[0];
    /* Delegate. */
    if (i < nb_entries) {
        tag_u32_enter(r);
        int rv = -1;
        if (r->nb_args >= map[i].nb_args) {
            rv = map[i].handle(r);
        }
        else {
            LOG("handle_tag_u32_map: nb_args = %d, expected %d\n",
                r->nb_args, map[i].nb_args);
        }
        tag_u32_leave(r);
        return rv;
    }
    return handle_tag_u32_map_meta(r, map, nb_entries);
}

/* Dynamic directory with identical substructure, i.e. a dynamic list
   of objects that all behave the same.  Not that in many cases, the
   path contains all the context information that is necessary and
   upper paths can be used. */
/* Still exploring this abstraction...
   See mod_bpmodular.c and hy1.c for examples. */

/* FIXME: This does not support nb_arg checking as is done in the
   static maps.  Maybe just not do it at all?  Because that's quite
   confusing. */

int handle_tag_u32_map_dynamic(struct tag_u32 *req,
                tag_u32_handle_fn sub,
                map_ref_fn fn, void *ctx) {
    if (req->nb_args < 1) {
        LOG("handle_tag_u32_map_dynamic: missing arg\n");
        return -1;
    }
    if (req->args[0] == TAG_U32_CTRL) {
        /* Dynamically generated instance map. */
        return handle_tag_u32_map_ref_meta(req, fn, ctx);
    }
    else {
        /* Note that unlike metadata, the direct index path does not
           check for invalid dynamic indices.  Those need to be
           checked in the leaf handlers. */
        tag_u32_enter(req);
        int rv = sub(req);
        tag_u32_leave(req);
        return rv;
    }
}

void send_reply_tag_u32_status_cstring(
    const struct tag_u32 *req, uint32_t status, const char *string) {
    const struct tag_u32 s = {
        .args = &status, .nb_args = 1,
        .bytes = string ? (const uint8_t*)string : 0,
        .nb_bytes = string ? strlen(string) : 0
    };
    send_reply_tag_u32_maybe(req, &s);
}

