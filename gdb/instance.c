#include "instance.h"
#include <stdint.h>

/* Components:

   - linker script links all ".instance" sections together, yielding
     an instance array.

   - this allows mapping a pointer to an index, to keep track of what
     was initialized using a minimal amount of RAM.

   - individual init functions will call init_need(), which will
     manage proper startup without duplication.
*/

/* FIXME: Currently only 32 instances are supported. */
uint32_t instance_initialized;
void instance_need(const struct instance *instance) {
    int n = &_instance_endx - &_instance_start;
    int i = instance - &_instance_start;
    if ((i >= 0) && (i < n)) {
        if (!(instance_initialized && (1 << i))) {
            instance->init();
            instance_initialized |= (1 << i);
        }
    }
}
