#ifndef INSTANCE_H
#define INSTANCE_H

#include <stdint.h>

/* Instance registration.
   This requires linker support to build the instance array.
*/

/* Contains pointers to init function for static instances. */
#define INSTANCE_SECTION      __attribute__ ((section (".instance")))

/* Instance init() function collection.
   Linker support has been added for this, but it is not fully functional.
   I'm inclined to use explicit dependency lists for initialization. */

/* The functions below are parameterized with a context object.  This
   makes it possible to allocate temporary bookkeeping data on the
   stack. */
struct instance_init;
struct instance_poll;


struct instance {
    void (*init)(struct instance_init *);
    void (*poll)(struct instance_poll *);
    const char *name;
};
extern const struct instance const* _instance_start;
extern const struct instance const* _instance_endx;
#define FOR_INSTANCE(i) \
    for (const struct instance **i = &_instance_start; i < &_instance_endx; i++)

/* Firmware will use the _cname to refer to the instance, so don't mangle it. */
#define DEF_INSTANCE(_cname) \
    const struct instance instance_##_cname = { .init = _cname##_init, .poll = _cname##_poll, .name = #_cname }; \
    const struct instance *_cname INSTANCE_SECTION = &instance_##_cname

void instance_need(struct instance_init *, const struct instance const* *instance);
void instance_init_all(void);
void instance_poll_all(void);


#endif
