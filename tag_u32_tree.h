#ifndef TAG_U32_TREE
#define TAG_U32_TREE

#include "tag_u32.h"

/* Basic idea:
   TAG_U32 represents a path view of a node in a tree.
   What is the corresponding tree view?

   E.g. how to canonically bundle a sequence of TAG_U32 messages in a
   single TAG_U32?

   More context:

   - Assume all we need to do is to represent nested trees

   - Assume there is a central naming scheme that allows each path
     component of that tree to be represented by a u32.

   In that view, TAG_U32 is a "sweet spot protocol".  It just misses a
   canonical tree encoding.


   This might not really need any code.

   Encode it as:
   - begin, commit
   - push / pop to go into sub-trees

*/


#endif
