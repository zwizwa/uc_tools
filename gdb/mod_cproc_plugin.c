#ifndef MOD_CPROC_PLUGIN
#define MOD_CPROC_PLUGIN

#include <stdint.h>
#include "tag_u32.h"

/* This is requred for cproc modules: the Erlang code uses TAG_U32 to
   perform I/O. */
int handle_tag_u32(const struct tag_u32 *);
#define HANDLE_TAG_U32 handle_tag_u32

#include "cproc.h"
#include "hw_cproc_stm32f103.h"
#include "mod_plugin.c"

w cproc_input[CPROC_NB_INPUTS];
void cproc_update(w *input, w changed);

int handle_tag_u32(const struct tag_u32 *s) {
    if ((s->nb_args == 2) &&
        (s->nb_bytes == 0)) {
        uint32_t i = s->args[0];
        uint32_t v = s->args[1];
        if (i < CPROC_NB_INPUTS) {
            // infof("cproc_input %d %d\n", i, v);
            cproc_input[i] = v;
            cproc_update(cproc_input, -1);
            return 0;
        }
    }
    infof("ERROR: mod_cproc_plugin: handle_tag_u32\n");
    return -1;
}

void cproc_output(uint32_t index, w value) {
    // infof("output %d %d\n", index, value);
    SEND_TAG_U32(index, value);
}

void plugin_start(void) {
    plugin_init_memory();
    infof("plugin_start %s\n", __FILE__);
}


#endif
