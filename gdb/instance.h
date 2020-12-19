#ifndef INSTANCE_H
#define INSTANCE_H

/* Note that this requires linker script support to create the global
   instance array. */

/* Contains pointers to init function for static instances. */
#define INSTANCE_SECTION      __attribute__ ((section (".instance")))

/* Instance init() function collection.
   Linker support has been added for this, but it is not fully functional.
   I'm inclined to use explicit dependency lists for initialization. */
struct instance {
    void (*init)(void);
};
extern const struct instance const* _instance_start;
extern const struct instance const* _instance_endx;
#define FOR_INSTANCE(i) \
    for (const struct instance **i = &_instance_start; i < &_instance_endx; i++)

/* Firmware will use the _cname to refer to the instance, so don't mangle it. */
#define DEF_INSTANCE(_cname) \
    const struct instance instance_##_cname = { .init = _cname##_init }; \
    const struct instance *_cname INSTANCE_SECTION = &instance_##_cname

void instance_need(const struct instance const* *instance);


#endif
