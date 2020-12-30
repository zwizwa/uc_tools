#ifndef MOD_CPROC_PLUGIN
#define MOD_CPROC_PLUGIN

#include <stdint.h>

/* This is requred for cproc modules: the Erlang code uses TAG_U32 to
   perform I/O. */
int tag_u32_handle(
    void *context,
    const uint32_t *args, uint32_t nb_args,
    const uint8_t *bytes, uint32_t nb_bytes);
#define TAG_U32_HANDLE tag_u32_handle

#include "mod_cproc.c"
#include "mod_plugin.c"
#define LET LET_STATIC

w cproc_input[CPROC_NB_INPUTS];
void cproc_update(w *input);

int handle_tag_u32(
    void *context,
    const uint32_t *arg,  uint32_t nb_args,
    const uint8_t *bytes, uint32_t nb_bytes) {

    if ((nb_args == 2) &&
        (nb_bytes == 0)) {
        uint32_t i = arg[0];
        uint32_t v = arg[1];
        if (i < CPROC_NB_INPUTS) {
            cproc_input[i] = v;
            cproc_update(cproc_input);
            return 0;
        }
    }
    return -1;
}

void plugin_start(void) {
}


#endif
