/* Generic macros used in stm and linux code. */

#ifndef UC_TOOLS_MACROS_H
#define UC_TOOLS_MACROS_H

/* Put this here, as macros.h seems to have become the de-facto
   uc_tools interface, and we want to limit surprises that are caused
   by variable struct lengths caused by debug macros. */
#include "uc_tools_config.h"

/* GENSYM, CONCAT */
#include "gensym.h"

/*  Only LOG is custom atm.  */
#ifdef __linux__
#include "os_linux.h"
#ifndef LOG
#define LOG(...) fprintf(stderr, __VA_ARGS__)
#endif
/* The default error propagation is to exit the program.  This works
 * well for the intended use as Erlang port. */

/* Uncomment this to disable trap.
   It seems to be a bad idea to enable this by default.  Why? */
//#define TRAP

#ifndef TRAP
int kill(pid_t pid, int sig);
#define TRAP kill(getpid(), SIGTRAP);
#endif

#ifndef ABORT
#define ABORT TRAP exit(1)
#endif

#elif defined(__CHIBIOS_RT__)
#include "os_macros_chibios.h"

#else
#ifndef LOG

/* FIXME: declaring this or including infof.h will bloat bootloader by
 * 750 bytes.  Why? */
// #include "infof.h"
// int infof(const char *fmt, ...);
#define LOG(...) infof(__VA_ARGS__)
#endif // __linux__

/* In bare metal microcontroller context there is no universal way to
   handle this so we take the following approach: implement abort a
   halt, by performing an endless loop.  This will trigger the
   watchdog timer. */
static inline void abort_busyloop(void) {
  loop:
    goto loop;
}
#ifndef ABORT
#define ABORT abort_busyloop()
#endif

#endif



#ifndef ARRAY_SIZE
#define ARRAY_SIZE(x) \
    (sizeof(x)/sizeof(x[0]))
#endif

/* Two array iterators.  Note that these are very different, so are
   named completely differently. */

/* The first one is for sized array types.  We do not have a way to
   check that a is an array and not a pointer, so be careful. */
#define FOR_ARRAY(_array,_el) \
    for(__typeof__(&_array[0]) _el = &_array[0]; _el < &_array[ARRAY_SIZE(_array)]; _el++)

#define FOR_ARRAY_INDEX(_array,_i) \
    for(uintptr_t _i = 0; _i < ARRAY_SIZE(_array); _i++)

/* The second one works for a pointer to a null terminated array.
   This could be an array of pointers, or an array of integers.
   Doesn't matter, as long as the terminator evaluates to false. */
#define UNTIL_NULL(_ptr,_el) \
    for(__typeof__(_ptr[0]) *_el = &_ptr[0]; *_el; _el++)

/* This is useful for arrays collected by the linker.  See e.g. command.h */
#define FOR_START_ENDX(_start, _endx, _p) \
    for (__typeof__(_start) _p = _start; _p < _endx; _p++)



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

#ifndef PACKEDas
#define PACKED __attribute__((__packed__))
#endif


#ifndef ERROR
#define ERROR(...) do {LOG(__VA_ARGS__);ABORT;}while(0)
#endif

#ifndef ASSERT
#define ASSERT(assertion) ({ \
            if(!(assertion)) { \
                ERROR("%s: %d: ASSERT FAIL: " #assertion "\n", __FILE__, __LINE__); \
            } })
#endif
#ifndef ASSERT_EQ
#define ASSERT_EQ(a,b) ({ \
            __typeof__(a) _a = (a); \
            __typeof__(b) _b = (b); \
            if(_a != _b) { \
                ERROR("ASSERT FAIL: " #a "(%d) == " #b "(%d)\n", (int)_a, (int)_b); \
            } })
#endif
#ifndef ASSERT_GT
#define ASSERT_GT(a,b) ({ \
            __typeof__(a) _a = (a); \
            __typeof__(b) _b = (b); \
            if(_a <= _b) { \
                ERROR("ASSERT FAIL: " #a "(%d) <= " #b "(%d)\n", _a, _b); \
            } })
#endif

#ifndef ASSERT_ERRNO
#define ASSERT_ERRNO(a) ({ \
            __typeof__(a) _a = (a); \
            if(-1 == (_a)) { \
                ERROR("ASSERT FAIL: " #a ", errno = %d, %s\n", errno, strerror(errno)); \
            } })
#endif


#ifndef OFFSETOF
#define OFFSETOF(typ,fld) ((uintptr_t)(&(((typ*)(0))->fld)))
#endif

// FIXME: pick a better name
#define STRUCT_FROM_FIELD(typ,fld,fld_ptr) ({ \
    void *v_fld_ptr = fld_ptr; \
    typ *par_ptr = v_fld_ptr - OFFSETOF(typ,fld); \
    par_ptr; })

#define NB_ARGS(...)  (sizeof((const char *[]){#__VA_ARGS__})/sizeof(int))

// I'm allowed.  I've typed this enough now.
#define ZERO(ptr) memset(ptr, 0, sizeof(*ptr))


/* Use for(;;) to run a block of code in a context that is set up /
   torn down by pre and post statements.  E.g.
   #define WITH_LOCK(l) WITH_PRE_POST(lock(l),unlock(l))
   WITH_LOCK(&my_lock) { my_critical_section_code(); }
*/
#define WITH_PRE_POST(env,pre,post) \
    for (int _just_once = ({pre(env); 0;}); _just_once<1; ({post(env); _just_once++;}))



#ifndef likely
#define likely(x)      __builtin_expect(!!(x), 1)
#endif
#ifndef unlikely
#define unlikely(x)    __builtin_expect(!!(x), 0)
#endif

#endif
