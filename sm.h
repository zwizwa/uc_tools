/* To the extent possible under law, Tom Schouten has waived all
   copyright and related or neighboring rights to sm.h
   Code:    http://zwizwa.be/git/uc_tools
   License: http://creativecommons.org/publicdomain/zero/1.0 */

#ifndef SM_H
#define SM_H

#include <stdint.h>

/* SM: Composable State Machines in C.
   A.k.a. recursive protothreads. */


/* RATIONALE:

   Most state machines encountered "in the wild" in embedded software
   systems are linear sequences of code, possibly with some form of
   early abort in case of errors.

   In the presence of an RTOS, these would be most conveniently
   expressed as blocking tasks, possibly with nested subroutines.

   However, when an RTOS is not available or appropriate, the
   abstraction in this file can lift some of the nuisance associated
   with implementing state machines manually (E.g. using explicit
   switch statements and/or function pointers).

   It implements abstractions for:

   - Waiting on a condition to be satisfied (SM_WAIT).

   - Code reuse for re-occuring subtasks (SM_CALL).


   The abstractions allow operation both from a main event loop
   polling routine calling a main _tick() method, and a ISR, e.g. to
   _tick() only when an event is there.

   A note on SM_CALL: Instead of using a call stack (as is done in the
   RTOS task case), the caller of a subroutine (submachine) manages
   its callee state explicitly as a constituent of its own state.

*/


/* IMPLEMENTATION:

   The implementation uses some GCC extensions:

   - computed goto / GCC labels as values.
     https://gcc.gnu.org/onlinedocs/gcc/Labels-as-Values.html

   - the __COUNTER__ macro to implement GENSYM.
     https://gcc.gnu.org/onlinedocs/cpp/Common-Predefined-Macros.html

   - statement expressions:
     https://gcc.gnu.org/onlinedocs/gcc-3.1/gcc/Statement-Exprs.html

   At the expense of convenience, it is possible to use switch()
   statements and manual label generation to translate this to
   portable code.



   A state machine consists of:

   - a struct with a "next" member to contain the C code resume point
   - a _init() method to initialize the struct
   - a _tick() method to perform the next action, containing
   - a "halt" label (pointing to SM_HALT) if SM_WAIT_TICK is used

   These _tick() methods have a body that looks like:

   {
     SM_RESUME(sm);
     // ... C code with SM_WAIT(sm, ...) statements
   halt:
     SM_HALT(sm);
   }

   SM_RESUME() enters the state machine, executing the following C
   code statement if the machine was resently initialized, or resuming
   where it left off in a previous call to _tick().

   An SM_WAIT() statement will evaluate a condition and if it is
   false, it will exit the _tick() method after puttimg the machine in
   a state such that the condition is re-evaluated the next time
   _tick() is executed.  In this case _tick() returns SM_WAITING.

   SM_HALT() loops forever in halting state, signalling this condition
   by the SM_HALTED return value.


   Caveat: Take specoal care in using local/automatic variables inside
   a _tick() function.  These are only valid inbetween suspension
   points.  However, GCC should produce warnings of uninitialized
   variables.

*/

#include "macros.h"


/* Return codes.  Other values are used to signal errors. */
typedef uint32_t sm_status_t;
#define SM_HALTED     ((sm_status_t)0)
#define SM_READY      ((sm_status_t)0xFFFFFFFEUL)  // yield point; caller can fetch output.
#define SM_WAITING    ((sm_status_t)0xFFFFFFFFUL)  // machine is still active, poll again

/* sm->next contains the address of the C code resume point. */
#define SM_RESUME(sm) do {                      \
    if (sm->next) goto *(sm->next) ; } while(0)
#define _SM_WAIT(sm,label,condition) do {       \
    label:                                      \
      if (!(condition)) {                       \
          sm->next = &&label;                   \
          return SM_WAITING;                    \
      }} while (0)
#define SM_WAIT(sm,condition)                   \
    _SM_WAIT(sm,GENSYM(label_),condition)

/* inverted. */
#define SM_WHILE(sm,ncond) \
    SM_WAIT(sm,!(cond))



#define _SM_HALT(sm, halt_loop)                 \
  do {                                          \
  halt_loop:                                    \
    sm->next = &&halt_loop;                     \
    return SM_HALTED; } while(0)
#define SM_HALT(sm)                             \
    _SM_HALT(sm,GENSYM(label_))

/* cooperative scheduling yield point.
   If you need this, you're on the wrong track! */
#define _SM_SUSPEND(sm,label) do {              \
        sm->next = &&label;                     \
        return SM_WAITING;                      \
      label:                                    \
        if (0);                                 \
    } while (0)

#define SM_SUSPEND(sm)                          \
    _SM_SUSPEND(sm,GENSYM(label_))


/* Running sub-machines.

   A sub-machine is the state machine analogue of a sub-routine: it is
   another state machine that is initialized by its caller, "ticked"
   until it halts, upon which control is transferred back to its
   caller.

   The code below uses the return values of the _tick() method:

   - SM_WAITING  no error, waiting for condition (needs another tick())
   - SM_HALTED   no error, done running.  Resume execution of caller.
   - other       error code: propagate error

*/

/* Optionally abort on error.
   Note that the error is returned only once.
   Subsequent calls return SM_HALTED. */

#define SM_ERROR_HALT(sm,err) do {              \
        sm->next = &&halt;                      \
        return err;                             \
    } while(0)


#define _SM_WAIT_TICK(sm,label,tick,abort) ({   \
        uint32_t rv;                            \
      label:                                    \
        rv = tick;                              \
        switch(rv) {                            \
        case SM_HALTED:                         \
            break;                              \
        case SM_WAITING:                        \
            sm->next = &&label;                 \
            return rv;                          \
        default:                                \
            if (abort) {                        \
                SM_ERROR_HALT(sm, rv);          \
            }                                   \
            else break;                         \
        }                                       \
        rv;})

#define SM_WAIT_TICK(sm,tick,abort)             \
    _SM_WAIT_TICK(sm,GENSYM(label_),tick,abort)

/* Using SM_WAIT_TICK, provide a convenience routine that initializes
   a state machine with standard naming convention and runs it to
   completion or error. */
#define SM_CALL(sm,name,state,...) do {                 \
    name##_init(state, __VA_ARGS__);                    \
    SM_WAIT_TICK(sm,name##_tick(state),1); } while(0)

/* Same, but don't abort on error, returning error value instead. */
#define SM_CALL_CATCH(sm,name,state,...) ({     \
    name##_init(state, __VA_ARGS__);            \
    SM_WAIT_TICK(sm,name##_tick(state),0); })

/* SM_SUB is like SM_CALL, with the convention that the state for a
   sub machine is stored in a union meber "sub" in the parent's state
   struct.  All SM_SUB calls in the same _tick() function can use the
   same union, as they are mutually exclusive.  Since the return value
   of _tick() is always SM_HALTED, instead a pointer to the struct is
   returned as the value of the expression. */
#define SM_SUB(sm, name, ...) \
    ({ SM_CALL(sm, name, &((sm)->sub.name), __VA_ARGS__ );      \
       &((sm)->sub.name); })

/* Same, but don't abort on error, returning error value instead. */
#define SM_SUB_CATCH(sm, name, ...) \
    SM_CALL_CATCH(sm, name, &((sm)->sub.name), __VA_ARGS__ )


/* Run with busy-wait (for testing) */
#define SM_RUN_BUSYWAIT(sm, name, ...) ({               \
    name##_init(sm, __VA_ARGS__);                       \
    uint32_t rv;                                        \
    while (SM_WAITING == (rv = name##_tick(sm)));       \
    rv;})


/* The above is enough to implement a small "operating system".  Here
   we add some buffering abstractions. */


/* Buffer abstraction. */
union sm_ptr {
    uint8_t  *u8;
    uint16_t *u16;
    uint32_t *u32;
};
struct sm_buf {
    union sm_ptr next;
    union sm_ptr endx;
};

union sm_const_ptr {
    const uint8_t  *u8;
    const uint16_t *u16;
    const uint32_t *u32;
};
struct sm_const_buf {
    union sm_const_ptr next;
    union sm_const_ptr endx;
};



/* Abstract read/write operations */
#define SM_BUF_READ(sm_buf, type) \
    (*((sm_buf)->next.type)++)
#define SM_BUF_WRITE(sm_buf, type, data) do { \
        *((sm_buf)->next.type)++ = data; } while(0)


/* Blocking write to buffer.  Write as long as there is room.

   Note that it is more efficient to iterate over the buffer on the
   writing side than to yield on every element. */

#define SM_WAIT_BUF_WRITE(sm, sm_buf, type, data) do {            \
        SM_WAIT(sm, (sm_buf)->next.type < (sm_buf)->endx.type);   \
        SM_BUF_WRITE(sm_buf, type, data); } while(0)

#define SM_WAIT_BUF_READ(sm, sm_buf, type) ({                     \
        SM_WAIT(sm, (sm_buf)->next.type < (sm_buf)->endx.type);   \
        SM_BUF_READ(sm_buf, type); })



// TODO: Both the wait and the data transfer can be made abstract.
// Essentially, a write to a buffer is a write to another state
// machine.  Conversely for reads.


typedef uint32_t (*sm_tick_fn)(void*);


/* Wait for condition or until nb_tries counter expired.
   Returns the value of the condition.

   This implements a pattern in hw_i2c.h
   See also SM_WAIT_CC_TIMEOUT in cycle_counter.h

   If the polling frequency is known this can be used as a wall-clock timeout.
*/
#define SM_WAIT_COUNT(s, condition, nb_tries) ({                        \
            int _condition;                                             \
            SM_WAIT(s, (_condition = (condition)) || (0 == (--(nb_tries)))); \
            _condition; })

#endif


