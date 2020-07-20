/* Test the interaction of sm.h based state machines and the csp
   scheduler.  This is done using two tasks: a counter, and a
   processor that does at least one SM_SUB call.  Both are written in
   sm.h style.
*/

#include "macros.h"
#include "sm.h"
#include "csp.h"

struct counter {
    struct csp_task task;
    /* Task ends in a 0-length event array for which we provide
     * storage here. */
    struct csp_evt evt[1];
    /* It is our responsibility to represent the continuation. */
    void *next;
    int count;
};
csp_status_t counter_tick(struct counter *s) {
    SM_RESUME(s);
    for(;;) {
        CSP_SND(s, 0, s->count);
        s->count++;
    }
}
void counter_init(struct counter *s) {
    memset(s,0,sizeof(*s));
    s->task.resume = (csp_resume_f)counter_tick;
}


int main(int argc, char **argv) {
    return 0;
}
