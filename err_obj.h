#ifndef ERR_OBJ_H
#define ERR_OBJ_H

/* FIXME: A simplified version of this is in the unified os_error.h */


/* Context: I'm tired of the horrible unix hack where error codes are
   encoded as negative integers, and correct values represent
   non-negative integers.  As long as there is a single name space for
   the error codes then this is fine, e.g. unix errno, but in many
   applications there are several different types of error codes, and
   this inevitably leads to hard to debug translation errors.

   Requirements:

   - Errors should be type-checked, so no more casting one integer
     error representation to another without some form of type
     checking.  In C this means errors should be represented by
     pointers.

   - No memory management at the user side: a pointer to an error
     should be managed by the abstraction.  Note that it is perfectly
     possible to cast the pointers to integer error codes inside the
     abstraction.  Just the user should not know about this.

   - The "OK" condition is represented by boolean false, i.e.. the
     NULL pointer.

   - Errors that cannot be handled just need an error logger.  Note
     that this err_obj.h API is part of a limited OSAL that protects
     uc_tools from external API, so not all errors need to be wrapped.

   - Errors that are meaningful to handle just need a selector /
     accessor to extract some information about the error.  These can
     be represented by individual structs and methods.

   - Provide a wrapper API for the negative error, positive value
     convention.

     Note that the latter is hacky, but is provided to have a better
     impedance match with unix like APIs and to allow gradual
     refactoring by just replacing integer errors by pointers without
     the need for adding extra arguments to functions.

   - I'm not a fan of setjmp exceptions in C.  While they are
     convenient, they make it too easy to make resource management
     mistakes.  However in most cases it is ok to use a error jump
     label that uses a local error code. */

#include "os_types.h"
#include "macros.h"


static inline const void *err_obj_unpack(void *result, uintptr_t *pval) {
    intptr_t ival = (intptr_t)result;
    if (ival < 0) {
        /* It's an error, just return the original error code. */
        return result;
    }
    else {
        /* It's a wrapped value. */
        *pval = ival;
        return NULL;
    }
}

#define DEF_ERR_OBJ(prefix)                                             \
    struct prefix##_error  { }; typedef const struct prefix##_err *prefix##_error_t; \
    struct prefix##_result { }; typedef const struct prefix##_result *prefix##_result_t; \
    static inline prefix##_error_t prefix##_unpack(struct prefix##_result *r, uintptr_t *p) { \
        return err_obj_unpack(r, p);                                    \
    }                                                                   \
    /* This is the constructor.  Only use it inside the implementation. */ \
    static inline prefix##_error_t prefix##_result(intptr_t val) {      \
        return (prefix##_error_t)val;                                   \
    }

// DEF_ERR_OBJ(os_tcp)


#endif

