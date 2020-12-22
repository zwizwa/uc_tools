#include "instance.h"
#include <stdint.h>

#include "infof.h"

/* Individual init functions will call init_need(), which will
   manage proper startup without duplication.
*/

#include "memoize.h"

/* Callback for memoize_eval. */
void instance_eval(
    instance_init_t *ctx,
    const struct instance *instance) {

    instance->init(ctx);
    infof("init %s\n", instance->name);
}
void instance_need(
    instance_init_t *ctx,
    const struct instance *instance) {

    memoize_eval(
        ctx,
        (void*)instance,
        (memoize_eval_fn)instance_eval);
}

void instance_need_top(
    uintptr_t max_nb_instances,
    const struct instance *instance) {

    memoize_eval_top(
        max_nb_instances,
        (void*)instance,
        (memoize_eval_fn)instance_eval);
}

struct instance_poll {
};
void instance_poll_all(void) {
}


