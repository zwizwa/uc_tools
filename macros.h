/* Generic macros used in stm and linux code. */

#ifndef UC_TOOLS_MACROS_H
#define UC_TOOLS_MACROS_H

#include "gensym.h"

/*  Only LOG is custom atm.  The rest is defined in macros.h */
#ifdef __linux__
#include <sys/types.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <unistd.h>
#include <string.h>
#ifndef LOG
#define LOG(...) fprintf(stderr, __VA_ARGS__)
#endif
/* The default error propagation is to exit the program.  This works
 * well for the intended use as Erlang port. */
#ifndef ABORT
//#define TRAP
int kill(pid_t pid, int sig);
#define TRAP kill(getpid(), SIGTRAP);
#define ABORT TRAP exit(1)
#endif

#else
#ifndef LOG

/* FIXME: declaring this or including infof.y will bloat bootloader by
 * 750 bytes.  Why? */
// #include "infof.h"
// int infof(const char *fmt, ...);
#define LOG(...) infof(__VA_ARGS__)
#endif
/* This should be implemented in the main C file of firmware.  Find a
   good way to do this.  For now, this is just to make current
   refactoring step compile. */
#ifndef ABORT
#define ABORT for(;;)
#endif

#endif



#ifndef ARRAY_SIZE
#define ARRAY_SIZE(x) \
    (sizeof(x)/sizeof(x[0]))
#endif

#define FOR_ARRAY(a,p) \
    for(__typeof__(&a[0]) p = &a[0]; p < &a[ARRAY_SIZE(a)]; p++)

#ifndef MAX
#define MAX(a,b) ({ __typeof__(a) _a = (a); __typeof__(b) _b = (b); _a >= _b ? _a : _b;})
#endif

#ifndef MIN
#define MIN(a,b) ({ __typeof__(a) _a = (a); __typeof__(b) _b = (b); _a <= _b ? _a : _b;})
#endif


#define CT_ASSERT(name, pred) \
    typedef char nct_assert_##name[(pred) ? 1 : -1]
#define CT_ASSERT_STRUCT_SIZE(name, size) \
    CT_ASSERT(struct_##name, sizeof(struct name) == size)
#define CT_ASSERT_UNION_SIZE(name, size) \
    CT_ASSERT(union_##name, sizeof(union name) == size)

/* Note that "static inline" doesn't necessarily inline.  Adding this
   attribute will force GCC to inline the function even if
   optimization is off. */
#ifndef INLINE
#define INLINE static inline __attribute__((__always_inline__))
#endif

#ifndef ERROR
#define ERROR(...) do {LOG(__VA_ARGS__);ABORT;}while(0)
#endif

#ifndef ASSERT
#define ASSERT(assertion) ({ \
            if(!(assertion)) { \
                LOG("%s: %d: ASSERT FAIL: " #assertion "\n", __FILE__, __LINE__); \
                ABORT; \
            } })
#endif
#ifndef ASSERT_EQ
#define ASSERT_EQ(a,b) ({ \
            __typeof__(a) _a = (a); \
            __typeof__(b) _b = (b); \
            if(_a != _b) { \
                LOG("ASSERT FAIL: " #a "(%d) == " #b "(%d)\n", _a, _b); \
                ABORT; \
            } })
#endif
#ifndef ASSERT_GT
#define ASSERT_GT(a,b) ({ \
            __typeof__(a) _a = (a); \
            __typeof__(b) _b = (b); \
            if(_a <= _b) { \
                LOG("ASSERT FAIL: " #a "(%d) <= " #b "(%d)\n", _a, _b); \
                ABORT; \
            } })
#endif

#ifndef ASSERT_ERRNO
#define ASSERT_ERRNO(a) ({ \
            __typeof__(a) _a = (a); \
            if(-1 == (_a)) { \
                ERROR("ASSERT FAIL: " #a ", errno = %d, %s\n", errno, strerror(errno)); \
            } })
#endif


#endif
