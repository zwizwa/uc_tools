#ifndef MOD_LOG_POLY
#define MOD_LOG_POLY

#include "macros.h"
#include "stdarg.h"

/* A hack to implement polymorphic variadic arguments.

   The point here is to implement Arduino's Serial.print() /
   .println() without using C++.

*/

static inline void log_poly_w(uintptr_t w) {
    uintptr_t flash_base = 0x08000000;
    uintptr_t flash_size = 128 * 1024;
    if ((w >= flash_base) && (w < flash_base + flash_size)) {
        LOG("%s",(const char*)w);
    }
    else {
        LOG("%d",w);
    }
}

/* Use C varargs, with a sentinel. */
#include <stdarg.h>

#define LOG_POLY_END 0x08FFFFFF

void log_poly_impl(const char *postfix, ...) {
    va_list ap;
    va_start(ap, postfix);
    for(;;) {
        uintptr_t w = va_arg(ap, uintptr_t);
        if (LOG_POLY_END == w) break;
        log_poly_w(w);
    }
    va_end(ap);
    LOG("%s", postfix);
}

typedef void (*log_poly_t)(const char *, ...);


#endif

//#define print(...)   log_poly("",   LOG_POLY_ARGS(__VA_ARGS__))
//#define println(...) log_poly("\n", LOG_POLY_ARGS(__VA_ARGS__))
