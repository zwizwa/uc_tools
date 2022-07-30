#ifndef PTHREAD_COROUTINE_H
#define PTHREAD_COROUTINE_H

#include "macros.h"
#include "assert_read.h"
#include "assert_write.h"

/* Coroutine mechanism based on pthread.

   Originally designed to create a "main loop inverter" without need
   for complex synchronization mechanisms, as part of a multi-device
   emulator.
*/

struct pthread_coroutine {
    pthread_t mainloop;
    int wakeup[2];
};

/* Simple pipe-based syncrhonization mechanism. */
#define PTHREAD_COROUTINE_PIPE_READ_END  0
#define PTHREAD_COROUTINE_PIPE_WRITE_END 1
static inline void pthread_coroutine_wakeup(struct pthread_coroutine *cor) {
    uint8_t dummy = 0;
    assert_write(cor->wakeup[PTHREAD_COROUTINE_PIPE_WRITE_END], &dummy, 1);
}
static inline void pthread_coroutine_wait(struct pthread_coroutine *cor) {
    uint8_t dummy = 0;
    assert_read(cor->wakeup[PTHREAD_COROUTINE_PIPE_READ_END], &dummy, 1);
}
static inline void pthread_coroutine_init(
    struct pthread_coroutine *cor,
    void *(mainloop)(void *arg), void *arg) {

    ASSERT_ERRNO(pipe(cor->wakeup));
    // LOG("pipe %d %d\n", cor->wakeup[0], cor->wakeup[1]);

    if (mainloop) {
        ASSERT(0 == pthread_create(
                   &cor->mainloop,
                   NULL /* attr */,
                   mainloop, arg));
    }
    else {
        /* Optional, e.g. for using the main process thread as a
           coroutine. */
    }
}
/* Example: wake up other, then put self to sleep. */
static inline void pthread_coroutine_yield(
    struct pthread_coroutine *self, struct pthread_coroutine *other) {
    pthread_coroutine_wakeup(other);
    pthread_coroutine_wait(self);
}


#endif
