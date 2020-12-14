/* Configuration for bp2 board.
   Ad-hoc debugging board.

   The idea is to gather parameterizable bits in mod_lab.c and use
   this file or clones of this file to set up one-off test
   configurations.

*/


#define PRODUCT "bp2"
#include "mod_lab.c"

void setup(void) {
    SEND_TAG_U32(4,5,6);
}

void loop(void) {
}

// bp2 ! {send_u32,[1,2,3]}.
int handle_tag_u32(
    void *context,
    const uint32_t *arg,  uint32_t nb_args,
    const uint8_t *bytes, uint32_t nb_bytes) {

    SEND_TAG_U32(4, nb_args);

    return 0;
}


/* Play with this a bit, then unify it with the new exo dataflow
 * paradigm later, by using generated instantiation code. */
//#include "mod_bp2.c"

