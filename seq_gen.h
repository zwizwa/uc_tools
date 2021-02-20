#ifndef SEQ_GEN_H
#define SEQ_GEN_H

/* Background: In a lazy language like Haskell, it's possible to do a
   lot of tricks to convert between iteration patterns.

   For Erlang I wrote a couple of different ones as part of erl_tools,
   taking into account 1) the immutable nature of Erlang, and 2) the
   ability to use tasks inside an interation.

   Now for uc_tools there are even more constraints, but it would be
   interesting to see how much of this can generalize.  Basic ideas
   would be:

   - Instead of fold, use stateful iteration.

   - Use sm.h protothreads for task / coroutine abstractions.

   Some useful patterns:

   - Stateful generators that produce a sequence of pointers,
     terminated with null.

   - Same, but inverted: a sequence of calls to a callback,
     parameterized by a pointer, and terminated with null.

*/

/* The generator produces a pointer to a structure.  We cannot say a
   whole lot about the lifetime, so it is assumed that the pointer is
   alive until the next time the generator is popped. */
struct seq_gen;
typedef const void* (*seq_gen_pop)(struct seq_gen *);
struct seq_gen {
    seq_gen_pop pop;
};

#endif
