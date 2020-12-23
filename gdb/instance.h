#ifndef INSTANCE_H
#define INSTANCE_H

#include <stdint.h>

/* Idempotent instance initialization. */

/* Init is just a thin wrapper around memoized evaluation. */
#include "memoize.h"
typedef struct memoize_table instance_init_t;

struct instance {
    void (*init)(instance_init_t *);
    const char *name;
};

#define DEF_INSTANCE(_cname)                                            \
    const struct instance _cname = {                                    \
        .init  = _cname##_init,                                         \
        .name  = #_cname                                                \
    };                                                                  \

void instance_need(instance_init_t *ctx,  const struct instance *instance);
void instance_need_top(uintptr_t max_nb_instances, const struct instance *instance);


// FIXME
void instance_poll_all(void);


#endif
