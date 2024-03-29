#ifndef OS_THREAD_H
#define OS_THREAD_H

/* Threads with static allocation.

   For now we follow the model in ChibiOS, which uses a macro that takes the
   name of the trhead (which is represented as an array of words), and
   the size of the stack.

   FIXME: Validate this for at least FreeRTOS and Zephyr.
*/



// ChibiOS/RT
#if defined(__CHIBIOS_RT__)

#define OS_THREAD_STACK(name,nb) \
    thread_t *name##_thread;     \
    THD_WORKING_AREA(name,nb)
#define OS_THREAD_MAIN(name,arg) \
    THD_FUNCTION(name,arg)
#define OS_THREAD_RETURN() \
    return
#define OS_THREAD_START(name, main, arg) \
    name##_thread = chThdCreateStatic(name, sizeof(name), HIGHPRIO-1, main, arg)

typedef mutex_t os_mutex_t;
static inline void os_mutex_init(mutex_t *m) {
    chMtxObjectInit(m);
}
static inline void os_mutex_free(mutex_t *m) {
    // FIXME: Is there something to do to undo init?  We will need to
    // be able to do init,free,init,free,... on the same object.
}
static inline void os_mutex_lock(mutex_t *m) {
    chMtxLock(m);
}
static inline void os_mutex_unlock(mutex_t *m) {
    chMtxUnlock(m);
}

#define OS_THREAD_WAIT(name) \
    chThdWait(name##_thread)

// Posix
#else
#include "os_linux.h"

typedef pthread_mutex_t os_mutex_t;
static inline void os_mutex_lock(os_mutex_t *m)    { pthread_mutex_lock(m); }
static inline void os_mutex_unlock(os_mutex_t *m)  { pthread_mutex_unlock(m); }

static inline void os_mutex_init(os_mutex_t *m)    { pthread_mutex_init(m, NULL); }
static inline void os_mutex_destroy(os_mutex_t *m) { pthread_mutex_destroy(m); }


// Emulate static threads
#define OS_THREAD_STACK(name, nb) \
    pthread_t name


#define OS_THREAD_MAIN(name,arg) \
    void *name(void *arg)
#define OS_THREAD_START(name, main, arg) \
    pthread_create(&name, NULL, main, arg)

static inline void os_thread_exit(void *retval) __attribute__((noreturn));
static inline void os_thread_exit(void *retval)  {
    pthread_exit(retval);
}

#define OS_THREAD_FUNCTION(name,arg) void *name(void *arg)
#define OS_THREAD_RETURN() return NULL

#define OS_THREAD_WAIT(name) \
    pthread_join(name, NULL)


#endif


#endif
