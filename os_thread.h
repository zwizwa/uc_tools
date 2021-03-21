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
    THD_WORKING_AREA(name,nb)
#define OS_THREAD_MAIN(name,arg) \
    THD_FUNCTION(name,arg)
#define OS_THREAD_RETURN() \
    return
#define OS_THREAD_START(name, main, arg) \
    chThdCreateStatic(name, sizeof(name), HIGHPRIO-1, main, arg)

// Posix
#else
#include <pthread.h>

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

#endif


#endif
