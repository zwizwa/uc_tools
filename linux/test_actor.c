#include "actor.h"
#include "macros.h"
#include <stdint.h>

/* Single message send between two tasks to illustrate the idea behind
 * the actor abstraction. */

/* Both tasks use the same state struct, but in essence this is
   private and can be different for each task. */
struct test_task {
    actor_task_t task;
    void *next;
    actor_task_t *other;
};

/* The first task prints all received messages. */
void task1_resume(actor_scheduler_t *s, struct test_task *t) {
    LOG("task1 resume\n");
    ACTOR_RESUME(t);
    for(;;) {
        actor_element_t msg = ACTOR_RECV(t);
        LOG("task1 receive = %d\n", msg.up);
    }
}

/* The second task sends a message and halts. */
void task2_resume(actor_scheduler_t *s, struct test_task *t) {
    LOG("task2 resume\n");
    actor_element_t msg = { .up = 123 };
    LOG("task2 send = %d\n", msg.up);
    actor_send(s, t->other, msg);
    actor_halt(s, &t->task);
}

static inline void log_dlist(struct dlist *head) {
    //LOG("dlist %p\n", head);
    for (struct dlist *l = head->next; l != head; l=l->next) {
        LOG("- %p\n", l);
    }
}


/* All allocation is done at the caller side to allow for maximum
   allocation flexibility, and simplicity of implementation in
   ns_actor.h */
void test1(actor_scheduler_t *s) {
    int nb_msg = 10;
    actor_element_t mbuf1[nb_msg], mbuf2[nb_msg];
    struct test_task task1 = {
        .task = ACTOR_TASK_INIT(task1.task, task1_resume, mbuf1)
    };
    struct test_task task2 = {
        .task = ACTOR_TASK_INIT(task2.task, task2_resume, mbuf2),
        .other = &task1.task
    };

    /* Note that for spawn and send operations, the scheduler is not
       run as part of the call, as this also needs to work inside a
       scheduling call and we do not want to re-enter the
       scheduler. */

    /* Spawn tasks and run. */
    actor_spawn(s, &task1.task);
    actor_spawn(s, &task2.task);
    actor_schedule(s);

    /* Push data into the network from the outside.  First queue two
     * messages and run. */
    actor_element_t msg = { .up = 456 };
    actor_send(s, &task1.task, msg); msg.up++;
    actor_send(s, &task1.task, msg); msg.up++;
    actor_schedule(s);

    /* Then three. */
    actor_send(s, &task1.task, msg); msg.up++;
    actor_send(s, &task1.task, msg); msg.up++;
    actor_send(s, &task1.task, msg); msg.up++;
    actor_schedule(s);

    LOG("cold:\n"); log_dlist(&s->cold);
    LOG("hot:\n");  log_dlist(&s->hot);
    LOG("dead:\n"); log_dlist(&s->dead);

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

