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
extern const struct instance _instance_start;
extern const struct instance _instance_endx;
#define FOR_INSTANCE(i) \
    for (const struct instance *i = &_instance_start; i < &_instance_endx; i++)
#define DEF_INSTANCE(name) \
    const struct instance instance_##name INSTANCE_SECTION = { \
        .init = name##_init,                                   \
    }


#endif
