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
   use a compiler.  E.g. use a finally tagless approach.
*/



#endif
