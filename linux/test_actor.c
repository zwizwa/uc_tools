#include "actor.h"
#include "macros.h"
#include <stdint.h>



struct test_ctx {
    actor_task_t *other;
};

/* Bootstrapping order is important.  We start the reader task1 first,
   so that is the one that has to sit and wait. */
void task1_resume(actor_scheduler_t *s, actor_task_t *t) {
    LOG("task1 resume\n");
    for(;;) {
        if (!actor_elements(t->mbox)) return;
        actor_element_t msg = actor_get(t->mbox);
        LOG("task1 receive = %d\n", msg.up);
    }
}


/* Sender task. */
void task2_resume(actor_scheduler_t *s, actor_task_t *t) {
    LOG("task2 resume\n");
    struct test_ctx *ctx = t->ctx;
    actor_element_t msg = { .up = 123 };
    ASSERT(ctx->other); // this is guaranteed due to start order
    LOG("task2 send = %d\n", msg.up);
    actor_send(s, ctx->other, msg);
}

void test1(actor_scheduler_t *s) {
    int nb_msg = 10;
    struct test_ctx ctx1={}, ctx2={};
    actor_element_t mbuf1[nb_msg], mbuf2[nb_msg];
    actor_queue_t
        mbox1 = { .buf = mbuf1, .size = nb_msg },
        mbox2 = { .buf = mbuf2, .size = nb_msg };

    /* There is a complication here: actors will need to be
       provisioned with pointers to other actors. */
    ctx2.other = actor_spawn(s, &mbox1, task1_resume, &ctx1);
    ctx1.other = actor_spawn(s, &mbox2, task2_resume, &ctx2);
}

void with_scheduler(int nb_task, void (*f)(actor_scheduler_t *)) {
    LOG("with_scheduler %d\n", nb_task);
    actor_task_t tasks[nb_task];
    actor_scheduler_t s;
    actor_scheduler_init(&s, tasks, nb_task);
    log_dlist(&s.hot);
    log_dlist(&s.cold);
    log_dlist(&s.dead);
    test1(&s);
}

int main(int argc, char **argv) {
    LOG("test_actor.c begin\n");
    with_scheduler(4, test1);
    LOG("test_actor.c end\n");
   return 0;
}

