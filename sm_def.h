#ifndef SM_DEF_H
#define SM_DEF_H

#include "sm.h"

/* Some macros to generate the boilerplate necessary to define sm
   structs and init functions.  This uses the following conventions:

   - 'arg' contains the arguments to _init().

   - 'state' contains extra state variables, which is initialized to 0.

   - 'sub' contains all the sub machines to which the _tick() method
     will perform SM_SUB calls.

   This is limiting, but allows for some abstraction for the otherwise
   cumbersome notation.

*/

/* Genreate state struct. */

#define SM_DEF_FIELD(typ,nam) typ nam;
#define SM_DEF_FIELDS(for_fields) for_fields(SM_DEF_FIELD)

#define SM_DEF_STATE(name)                                \
    struct name##_state {                                 \
        void *next;                                       \
        struct { SM_DEF_FIELDS(name##_arg)   } arg;       \
        union  { SM_DEF_FIELDS(name##_sub)   } sub;       \
        struct { SM_DEF_FIELDS(name##_state) } state;     \
    };                                                    \

/* Generate init function. */

#define SM_DEF_DECL_ARG(typ,nam) ,typ nam
#define SM_DEF_DECL_ARGS(for_fields) for_fields(SM_DEF_DECL_ARG)

#define SM_DEF_DO_INIT(typ,nam) s->arg.nam = nam;
#define SM_DEF_DO_INITS(for_fields) for_fields(SM_DEF_DO_INIT)

#define SM_DEF_INIT(name)                               \
    void name##_init(struct name##_state *s             \
                     SM_DEF_DECL_ARGS(name##_arg)       \
        ) {                                             \
        memset(s,0,sizeof(*s));                         \
        SM_DEF_DO_INITS(name##_arg)                     \
    }

/* Generate blocking wrapper. */

#define SM_DEF_REF_ARG(typ,nam) ,nam
#define SM_DEF_REF_ARGS(for_fields) for_fields(SM_DEF_REF_ARG)

#define SM_DEF_BUSYWAIT(name)                                   \
    sm_status_t name##_busywait(struct name##_state *s          \
                         SM_DEF_DECL_ARGS(name##_arg) ) {       \
        name##_init(s SM_DEF_REF_ARGS(name##_arg));             \
        sm_status_t rv = 0;                                     \
        while (SM_WAITING == (rv = name##_tick(s)));            \
        return rv;                                              \
    }


#endif
