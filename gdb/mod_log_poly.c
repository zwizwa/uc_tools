#ifndef MOD_LOG_POLY
#define MOD_LOG_POLY

#include "macros.h"
#include "stdarg.h"

// FIXME: This doesn't work without disabling -Werror

/* FIXME: make it work with stdarg anyway.  If this uses Flash
   addresses to implement approximate polymorphy, then it's also ok to
   add an in-band terminator. */

/* This is a terrible hack to implement polymorphic variadic arguments
   as used in Arduono's Serial.print() / .println().  But the trick
   itself might be useful in a more controlled setting.  For
   posterity...

   - Using a temp array instead of stdarg.h because the latter is not
     self-terminating, i.e. we can't see how many arguments were
     passed.  For arrays we can use sizeof().
*/



// FIXME: Doesn't actually work due to zero-argument println()
typedef void* log_poly_arg_t;
struct log_poly_args {
    uintptr_t v[1];
    int c;
} log_poly_args;
typedef void (*log_poly_t)(const char *postfix, const struct log_poly_args *args);
void log_poly(const char *postfix, const struct log_poly_args *args) {
    uintptr_t flash_base = 0x08000000;
    uintptr_t flash_size = 128 * 1024;
    for (int i=0; i<args->c; i++) {
        uintptr_t w = args->v[i];
        if ((w >= flash_base) && (w < flash_base + flash_size)) {
            LOG("%s",(const char*)w);
        }
        else {
            LOG("%d",w);
        }
    }
    LOG("%s", postfix);
}
#define LOG_POLY_ARGS(...) ({                                   \
            log_poly_arg_t args[] = { __VA_ARGS__ };                 \
            log_poly_args.c = ARRAY_SIZE(args);                 \
            memcpy(log_poly_args.v, args, sizeof(args));        \
            &log_poly_args; /* dummy*/  })


#endif

//#define print(...)   log_poly("",   LOG_POLY_ARGS(__VA_ARGS__))
//#define println(...) log_poly("\n", LOG_POLY_ARGS(__VA_ARGS__))
