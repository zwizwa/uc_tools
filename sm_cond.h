#ifndef SM_COND_H
#define SM_COND_H

/* Translate conditions into wakeup / retry events.

   First, the focus here is simplicity of implementation.  There might
   be more complex and efficient solutions, but simplicity is the main
   goal.

   The observation: inter-task synchronization is much easier to
   express if it is condition-based instead of event-based.

   E.g.

       wait(gpio1 == 0)

   or

       wait(T >= T_timeout)

   or a combination of these

       wait((gpio1 == 0) or (T >= T_timeout))


   To schedule something like that in a naive way, you could use a
   round robin continously polling scheduler.  That is the original
   context in which sm.h protothreads are developed.

   However, I have run into some use cases where I need a more direct
   response, scheduling protothreads from an interrupt routine.

   So I went on a search for an algorithm to convert a polling routine
   into events, and what I end up with is that 1. conditions need to
   be made expicit, and 2. some kind of dataflow form would be needed
   to compute the sensitivities (reverse dependency graph) of
   condition formulas.  Essentially, a derivative.  So long story
   short: there is a straightforward formalism to look at this (value
   change of a boolean expression).  And there is a traditional way to
   repesent this (inversion of data flow graph).

   But... sm.h doesn't need that, becaus it already "hosts" the
   condition functions inside the protothread body, so the only thing
   that is really needed is a way to extract the wakeup events from a
   formula.

   That is what this header file does.  It assumes:

   - An abstract representation of all the events that a program
     supports

   - A way to turn evaluation of a condition into the generation of a
     list of events

   - Boolean composition is just C's boolean expressions.

   This is the same idea as "redo".

   One interesting corrolary is that this removes the need of a
   software timer.  The network can simply be polled until all
   conditions are false, and the occurance of a timeout condition
   simply records the _earliest_ timeout that is necessary to
   re-evaluate the waiting conditions.  This introduces some spurious
   polls, but completely removes the need for complex data structures
   to represent the conditions.  I.e. prefer compiled over
   interpreted.

   Additionally, tis allows for introduction of primitive events that
   do not have system interrupts associated to them, by simply polling
   them on a timer grid.

   So the summary:

   - Use conditions in the most natural way: expressed in C code.

   - Use the evaluation of those conditions to reconstruct wakeup events.

   - Be robust against spurious events (i.e. map them to nop polls).
     This is the added value of the abstraction, allowing events to be
     abstracted as conditions.

*/

#include <stdint.h>

/* This is mostly structured around the representation of time.
   Other events are implementation-dependent.

   Time is circular.  Most straightforward is to use the processor's
   cycle counter.  Note that hardware timers are used only for
   implementation of the wakeup mechanism, and the protothreads do not
   interact with timers directly. */

/* Represent this as composite type to make sure the C type checker
   can catch the difference betwen relative and absolute time.  This
   is a little inconvenient but I bet will save a lot of headache... */
typedef union { uint32_t abs; } sm_time_abs_t;
typedef union { int32_t  rel; } sm_time_rel_t;
#define SM_TIME_REL_MAX 0x7FFFFFFFUL


struct sm_context {
    /* Time takes a central role.  We can compute it once and pass it
       to every routine. */
    sm_time_abs_t now;

    /* This is initialized with a spurious poll at the max future
       relative time.  Every primitive condition that depends on a
       timeout can reduce this. */
    sm_time_rel_t retry;

    /* Implementation-dependent events.  When a primitive is
       evaluated, it needs to set one of the bits in this bitfield. */
    uint32_t event;

};
sm_time_rel_t sm_time_diff(sm_time_abs_t later, sm_time_abs_t sooner) {
    sm_time_rel_t dt = { .rel = later.abs - sooner.abs };
    return dt;
}
sm_time_abs_t sm_time_offset(sm_time_abs_t time, uint32_t offset) {
    sm_time_abs_t t = { .abs = time.abs + offset };
    return t;
}

/* This is what it is all about: when evaluating a timer, schedule a
   wakeup if it has not yet expired. */
int sm_timer_elapsed(struct sm_context *c, sm_time_abs_t timeout) {
    sm_time_rel_t remaining = sm_time_diff(timeout, c->now);
    if (remaining.rel <= 0) {
        /* Now is at or past the absolute timeout.  We do not need to
           reschedule.

           This condition will remain true as long as the time base
           doesn't wrap.  That is important when the condition is
           combined with other conditions: you don't want the timeout
           to flip sign waiting on the other event.

           In a typical high speed setup we use the cycle counter.  At
           72MHz on a F103 the wrap around point happens at 29.8 seconds.

           So it is not clear what to do here.  It would be possible
           to latch the value, but in practice it might always be
           possible to either change the resolution of the timer, or
           change the time base such that the resolution is adequate
           to handle small and large time scales.
        */
        return 1;
    }
    else {
        /* Now is before the absolute timeout.  Reschedule a poll. */
        if (c->retry.rel > remaining.rel) { c->retry = remaining; }
        return 0;
    }
}


#endif
