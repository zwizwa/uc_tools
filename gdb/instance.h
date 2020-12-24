#ifndef INSTANCE_H
#define INSTANCE_H

#include <stdint.h>
#include "macros.h"

/* Idempotent instance initialization. */

/* Init is just a thin wrapper around memoized evaluation. */
#include "memoize.h"
typedef struct memoize_table instance_init_t;
typedef uintptr_t instance_status_t;
struct instance {
    /* 0 == OK, other is error code. */
    instance_status_t (*init)(instance_init_t *);
    const char *name;
};

#define DEF_INSTANCE(_cname)                                            \
    const struct instance _cname = {                                    \
        .init  = _cname##_init,                                         \
        .name  = #_cname                                                \
    };                                                                  \

instance_status_t instance_need(instance_init_t *ctx,  const struct instance *instance);
instance_status_t instance_need_top(uintptr_t max_nb_instances, const struct instance *instance);


/* This performs early return if instances are not available. */
#define INSTANCE_NEED(ctx, ...) {                                       \
        const struct instance * _instances[] = { __VA_ARGS__, NULL};    \
        instance_status_t _status;                                      \
        UNTIL_NULL(_instances, _ptr) {                                  \
            if ((_status = instance_need(ctx, *_ptr))) return _status;  \
        }                                                               \
    }

#endif
