#ifndef OS_ERROR_H
#define OS_ERROR_H

/* TL;DR

   Representation of OS error codes.

   Note that in some cases there might be more than one error code
   representation.  We assume that it is possible to unify those codes
   at the OSAL boundary.  Example: ChibiOS with LwIP internal codes,
   and FatFS internal codes.
*/


/* Errors are always defined relative to the osal.  We don't know
   anything about them here.  We only know about the existence of
   "packed" error codes, e.g. those that can reprent both an error
   code and a result.

   FIXME: Currently it is assumed that packed error/result codes use
   the sign bit to distinguish between error and value.  Abstract it
   differently if this is no longer appropriate. */

#include <stdint.h>
#include "macros.h"

struct os_result;
struct os_error;

typedef const struct os_result *os_result_t;
typedef const struct os_error  *os_error_t;

#define OS_OK NULL

static inline os_result_t os_ok_result(intptr_t rv) {
    ASSERT(rv >= 0);
    return (os_result_t)rv;
}
static inline os_error_t os_error(intptr_t e) {
    ASSERT(e < 0);
    return (os_error_t)e;
}
static inline os_result_t os_error_result(os_error_t e) {
    return (os_result_t)e;
}
static inline os_error_t os_result_unpack(os_result_t r, uintptr_t *pval) {
    intptr_t i = (intptr_t)r;
    if (i >= 0) {
        /* It's a value. */
        *pval = i;
        return OS_OK;
    }
    else {
        /* Cast it to an error. */
        return (os_error_t)r;
    }
}


// const char *os_strerror(os_error_t e);
intptr_t os_interror(os_error_t e) { return (intptr_t)e; }

#define OS_LOG_ERROR(_tag, _e)                                  \
    LOG(_tag ": %d %s\n", os_interror(_e), os_strerror(_e))




/* Another use case: error unions.

   I want to be able to get error types right, i.e. to force proper
   casting of errors, but otoh. I do not want to spend too much time
   on writing code for things that are essentially mostly ignored.

   A the typical pattern is the following: the errors that a routine
   can produce are usually a combination of low level os errors, and
   (multiple types of) application prototocol errors.

   So how to quickly define a union type that still fits in an
   integer?  Typically an integer has plenty of space to encode all
   error codes, but the mapping can be quite arbitrary.


   A thing to consider: a global error code only works when it is
   possible to create a central registry, which means that you need
   full control over the error name space.

*/


#endif
