#define _POSIX_C_SOURCE 199309L
#include <time.h>

#include "sm_cond.h"
#include "sm.h"


void sleep_ms(int ms) {
    int sec = ms / 1000;
    int ms_rem = ms % 1000;
    struct timespec tv = {
        .tv_sec  = sec,
        .tv_nsec = ms_rem*1000*1000
    };
    nanosleep(&tv,NULL);
    LOG("sleep_ms(%d)\n", ms);
}

struct test {
    void *next;
    sm_time_abs_t timeout;
    uint32_t count;
};
void test_init(struct test *s) {
    memset(s,0,sizeof(*s));
}

/* Since there is often only one context, it's probably ok to make
   this global.  It saves 4 bytes in every struct as opposed to
   putting it there. */
struct sm_context ctx = {};

sm_status_t test_tick(struct test *s) {
    SM_RESUME(s);
    for(s->count = 0; s->count < 3; s->count++) {
        s->timeout = sm_time_offset(ctx.now, 1000);
        SM_WAIT(s, sm_timer_elapsed(&ctx, s->timeout));
        LOG("%d\n", s->count);
    }
    SM_HALT(s);
}

int main(void) {
    struct test s = {};
    /* Start by running it once. */
    test_init(&s);
    for(;;) {
        ctx.retry.rel = SM_TIME_REL_MAX;
        sm_status_t status;
        if (SM_WAITING != (status = test_tick(&s))) break;
        /* This is a good place to check if there are _any_ wakeup
           events at all. */
        sleep_ms(ctx.retry.rel);
        ctx.now.abs += ctx.retry.rel;
    }
    return 0;
}
