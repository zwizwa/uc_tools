#include "instance.h"
#include <stdint.h>

#include "infof.h"

/* Components:

   - linker script links all ".instance" sections together, yielding
     an instance array.

   - this allows mapping a pointer to an index, to keep track of what
     was initialized using a minimal amount of RAM.

   - individual init functions will call init_need(), which will
     manage proper startup without duplication.
*/



/* FIXME: Currently only 32 instances are supported.
   If more are needed, change the implementation to use a global
   pointer to the initialization vector, allocate that on the stack,
   and derive the size of the table from the size of the instance
   array.

   FIXME: This might be useful for systems where the instance
   gathering linker trick is not possible.  Here it is only used to
   map an instance to a number, so we can track state, but it could
   just as well be stored in the instance.

*/

struct instance_init {
    // FIXME: Make this into an array with configurable size.
    uint32_t initialized;
};

void instance_need(struct instance_init *init_state,
                   const struct instance const* *instance) {
    int n = &_instance_endx - &_instance_start;
    int i = instance - &_instance_start;
    if ((i >= 0) && (i < n)) {
        if (!(init_state->initialized & (1 << i))) {
            infof("init %s\n", (*instance)->name);
            (*instance)->init(init_state);
            init_state->initialized |= (1 << i);
        }
        else {
            //infof("have %s\n", (*instance)->name);
        }
    }
}

/* Call this only once per startup.  It keeps track of initialization
   state on the stack and so will forget about it after. */
void instance_init_all(void) {
    struct instance_init init_state = {};
    FOR_INSTANCE(i) {
        instance_need(&init_state, i);
    }
}

struct instance_poll {
};

void instance_poll_all(void) {
    struct instance_poll poll_state = {};
    FOR_INSTANCE(i) {
        (*i)->poll(&poll_state);
    }
}


