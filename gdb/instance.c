#include "instance.h"
#include <stdint.h>

#include "infof.h"

/* Individual init functions will call init_need(), which will
   manage proper startup without duplication.
*/

#include "memoize.h"

/* Callback for memoize_eval. */
instance_status_t instance_eval(
    instance_init_t *ctx,
    const struct instance *instance) {
    instance_status_t s = instance->init(ctx);
    infof("init %s", instance->name);
    if (s) { infof(": ERROR %x", s); }
    infof("\n");
    return s;
}
instance_status_t instance_need(
    instance_init_t *ctx,
    const struct instance *instance) {
    return
        memoize_eval(
            ctx,
            (void*)instance,
            (memoize_eval_fn)instance_eval);
}

instance_status_t instance_need_top(
    uintptr_t max_nb_instances,
    const struct instance *instance) {
    return
        memoize_eval_top(
            max_nb_instances,
            (void*)instance,
            (memoize_eval_fn)instance_eval);
}


