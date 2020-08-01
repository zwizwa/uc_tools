#include "actor.h"
#include "macros.h"
#include <stdint.h>



struct test_task {
    actor_task_t task;
    actor_task_t *other;
};

struct test_ctx {
    actor_task_t *other;
};

/* Bootstrapping order is important.  We start the reader task1 first,
   so that is the one that has to sit and wait. */
void task1_resume(actor_scheduler_t *s, actor_task_t *t0) {
    struct test_task *t = (void*)t0;
    LOG("task1 resume\n");
    for(;;) {
        if (!actor_elements(&t->task.mbox)) return;
        actor_element_t msg = actor_get(&t->task.mbox);
        LOG("task1 receive = %d\n", msg.up);
    }
}


/* Sender task. */
void task2_resume(actor_scheduler_t *s, actor_task_t *t0) {
    struct test_task *t = (void*)t0;
    LOG("task2 resume\n");
    actor_element_t msg = { .up = 123 };
    LOG("task2 send = %d\n", msg.up);
    actor_send(s, t->other, msg);
}

void test1(actor_scheduler_t *s) {
    int nb_msg = 10;
    actor_element_t mbuf1[nb_msg], mbuf2[nb_msg];
    struct test_task task1 = {
        .task = {
            .dlist  = DLIST_INIT(task1.task.dlist),
            .resume = task1_resume,
            .mbox   = { .buf = mbuf1, .size = nb_msg },
        },
    };
    struct test_task task2 = {
        .task = {
            .dlist = DLIST_INIT(task2.task.dlist),
            .resume = task2_resume,
            .mbox = { .buf = mbuf2, .size = nb_msg },
        }
    };
    task2.other = &task1.task;
    task1.other = &task2.task;

    actor_spawn(s, &task1.task);
    actor_spawn(s, &task2.task);

    LOG("cold:\n"); log_dlist(&s->cold);
    LOG("hot:\n");  log_dlist(&s->hot);

}

void with_scheduler(int nb_task, void (*f)(actor_scheduler_t *)) {
    LOG("with_scheduler %d\n", nb_task);
    actor_scheduler_t s;
    actor_scheduler_init(&s);
    test1(&s);
}

int main(int argc, char **argv) {
    LOG("test_actor.c begin\n");
    with_scheduler(4, test1);
    LOG("test_actor.c end\n");
   return 0;
}

