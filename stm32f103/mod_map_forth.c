#ifndef MOD_MAP_FORTH
#define MOD_MAP_FORTH

#include "command.h"

/* Provide a TAG_U32 directory structure for Forth commands.
   Commands are invoked as:
   - args[0]   = nb_args to pop as reply
   - args[1..] = to load on command stack before running command
*/

/* Commands */
int map_forth_op(struct tag_u32 *req) {
    uint32_t nb_replies = (req->nb_args > 0) ? req->args[0] : 0;
    uint32_t cmd_nb = req->args[-1];

    if (cmd_nb >= command_index_size()) return -1;
    for(uint32_t i=1; i<req->nb_args; i++) {
        command_stack_push(req->args[i]);
    }
    (*command_ref(cmd_nb))->run();
    uint32_t args[nb_replies];
    for(uint32_t i=0; i<nb_replies; i++) {
        args[nb_replies-i-1] = command_stack_pop();
    }
    const struct tag_u32 s = {
        .args = args, .nb_args = nb_replies,
    };
    send_reply_tag_u32_maybe(req, &s);
    return 0;
}
int map_forth_entry(struct tag_u32 *req, void *no_ctx,
                  struct tag_u32_entry *entry) {
    TAG_U32_UNPACK(req, 0, m, cmd_nb) {
        if (m->cmd_nb >= command_index_size()) return -1;
        const struct tag_u32_entry e = {
            .name = (*command_ref(m->cmd_nb))->name,
            .type = "cmd",
        };
        *entry = e;
        return 0;
    }
    return -1;
}
int map_forth(struct tag_u32 *req) {
    return handle_tag_u32_map_dynamic(req, map_forth_op, map_forth_entry, NULL);
}



#endif
