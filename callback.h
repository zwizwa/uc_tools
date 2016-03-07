/* To the extent possible under law, Tom Schouten has waived all
   copyright and related or neighboring rights to callback.h
   Code:    http://zwizwa.be/git/uc_tools
   License: http://creativecommons.org/publicdomain/zero/1.0 */

#ifndef CALLBACK_H
#define CALLBACK_H


#include <stdint.h>

/* Fat pointers / callbacks.

   Rationale: callbacks get passed around a lot.  It's easier to just
   use a single pointer (cb in the CB_CALL* macros below) and
   reconstruct the containing object using CB_OBJ below, instead of
   copying a structure that contains a pointer to function and
   context. */

#define CB_DEF(type)                            \
    struct cb_##type;                           \
    struct cb_##type { type (*fn)(void*); }
#define CB_DEF_P(type)                          \
    struct cb_##type##p;                        \
    struct cb_##type##p { type* (*fn)(void*); }

CB_DEF(int);
CB_DEF(uint32_t);
CB_DEF(void);
CB_DEF_P(void);

#define CB_CALL(cb) ((cb)->fn(cb))
#define CB_CALL_IF(cb) {if(cb){(cb)->fn(cb);}}

/* Obtain object pointer from method offset.  Here ptr points to a
   slot in the object struct that contains the pointer to the callback
   function. */
#define CB_OBJ(ptr, type, member) (((void*)ptr) - offsetof(type, member))


#endif //CALLBACK_H
