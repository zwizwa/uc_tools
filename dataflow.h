/* Push-style dataflow with pure update functions.

   Note: the purity is important in the implementation.  I.e. the
   nodes essentially represent continuous signals that are
   _implemented_ as discrete changes, but do not carry any other
   "event" semantics.

   Events in the traditional sense only exist at the boundaries:

   - A push routine allows changing the value of a node, essentially
     converting an event (the occurance of a function call at a
     particular instance) into a value change.

   - A notification routine allows monitoring the change of any node
     in the system, converting a value change back into the occurance
     of a function call at a particular instance.

   this means that if a push call or an internal computation doesn't
   actually change the _value_ of the node, no further propagation
   happens.

   This pure semantics has a number of advantages.  The main one being
   that it is easier to reason about what happens in a network, as
   there are no side effects.  The only thing we can do is change the
   input and monitor output changes.


   This code has been optimized for static compilation of the network,
   where only the node data structures are in RAM, and the rest of the
   metadata necessary to perform evaluation can be stored in Flash.


   The implementation uses a doubly-linked directed acyclic graph.
   See dataflow.lua for the implementation that this was modeled
   after.  Some future work is planned to combine dataflow.lua, the
   dfl.lua dataflow language evaluator / compiler, and this static
   evaluator to create a test framework and compiler for building pure
   dataflow networks.


   There was the idea to write the evaluator as a computed goto state
   machine.  This way a smaller stack can be used, and the evaluation
   can be paused in case it is somehow mixed with some other blocking
   evaluation mechanism.  However there are two different continuation
   types (forward and reverse propagation) which both need to be
   represented separately, so it is actually seems to be a lot easier
   to stick to plain C in a first implementation.
*/

#ifndef DATAFLOW_H
#define DATAFLOW_H
#include "macros.h"

#include <stdint.h>

union dataflow_value {
    // Type tag?  Maybe not necessary since nodes will likely be typed
    // in the description, and our main purpose is to write an
    // evaluator for a generated network.
    uintptr_t u;
    void *v;
};

struct dataflow_node;
typedef union dataflow_value (*dataflow_update_f)(struct dataflow_node *);
typedef void (*dataflow_notify_f)(struct dataflow_node *);

struct dataflow_meta {
    dataflow_update_f update;
    dataflow_notify_f notify;
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

void dataflow_push(
    struct dataflow_node *n,
    union dataflow_value v);

#endif
