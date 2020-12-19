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
*/
uint32_t instance_initialized;
void instance_need(const struct instance const* *instance) {
    int n = &_instance_endx - &_instance_start;
    int i = instance - &_instance_start;
    if ((i >= 0) && (i < n)) {
        if (!(instance_initialized && (1 << i))) {
            //infof("init %d %x\n", i, instance);
            (*instance)->init();
            instance_initialized |= (1 << i);
        }
        else {
            //infof("have %d %x\n", i, instance);
        }
    }
}
