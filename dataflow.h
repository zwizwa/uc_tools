#ifndef DATAFLOW_H
#define DATAFLOW_H
#include "macros.h"

/* Dataflow with pure update functions.

   What I want is support for push-style, i.e. react to incoming
   events, which would require partial pull-style as well.

   Goal again is to keep it simple, so focus on the data structure
   that is used when an event comes in at an input node.  Construction
   of the data structure can be done either at compile time, or at
   controller bootup.

   In a previous attempt (see "data elements" repo), I struggled with
   the idea of simultaneity of input events, which is something that
   happens even if you have fanout and merge in the network and no
   simultaneous external input events.  The basic idea is that there
   is a concept of a single time instance in which an update occurs.
   It is very important to realize this in the representation.

   So it appears that these datastructures are necessary:

   - A doubly-linked version of the DAG
   - A stack for evaluation
   - A tagging mechanism to indicate "todo in this phase"


   Some implementation choices:

   - There are going to be a lot of pointers, so implement them using
     indices into an array of nodes.  I'm thinking 8 or 16 bits, or
     even an intermediate representation of variable bit length.

   - Go for a static implementation first.  Something that can be
     generated from a higher level description and can go into Flash.


   See dataflow.lua for an implementation using a doubly linked graph.
   If the datastructure can be easily translated, the code translation
   should be straightforward.

   Now because the functions are pure, it might even be possible to
   use a compiler.  E.g. use a finally tagless approach.  EDIT: See
   dfl.lua

   With that out of the way, what is still necessary to create the
   forward and reverse dependencies?  They just need to be printed to
   C structs.  The evaluator can then be copied directly from lua
   code.  Maybe write that first, so it is clear what it should look
   like.

   I was thinking to write the evaluator as a computed goto state
   machine.  This way a smaller stack can be used, and the evaluation
   could be paused in case it is somehow mixed with some other
   blocking mechanism.  However there are a number of different
   continuation types that all need to be represented separately, so
   it is actually a lot easier to use C recursion.
*/

#include <stdint.h>

#define DATAFLOW_MAX_NB_NODES 10
union dataflow_value {
    // Type tag?  Maybe not necessary since nodes will likely be typed
    // in the description, and our main purpose is to write an
    // evaluator for a generated network.
    uintptr_t up;
    void *v;
};
struct dataflow_state;
struct dataflow_node;
typedef void (*dataflow_update_f)(struct dataflow_state *, struct dataflow_node *);
struct dataflow_meta {
    dataflow_update_f update;
    uint32_t nb_args;
    struct dataflow_node **fwd_deps, **rev_deps;
};
#define DATAFLOW_FOR_DEPS(deplist,ppnode) \
    for(struct dataflow_node **ppnode = deplist; *ppnode; ppnode++)

struct dataflow_node {
    const struct dataflow_meta *meta;
    union dataflow_value value;
    unsigned int valid:1;
    unsigned int initialized:1;
};
struct dataflow_state {
    struct dataflow_node *node;
};
void dataflow_push(
    struct dataflow_state *d,
    struct dataflow_node *n,
    union dataflow_value v);

#endif
