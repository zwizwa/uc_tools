/* To the extent possible under law, Tom Schouten has waived all
   copyright and related or neighboring rights to infof.h
   Code:    http://zwizwa.be/git/uc_tools
   License: http://creativecommons.org/publicdomain/zero/1.0 */

#include "macros.h"
#include "infof.h"
#include <stdarg.h>

// 20221103 The formatter is abstracted in an NS module
#define NS(name) CONCAT(info_,name)
// no context def/ref, context is global
#define info_CTX_DEF
#define info_CTX_REF
// The original routine did not use a context parameter.
typedef void *_info_ctx_t;
#include "ns_infof.c"
#undef NS


extern int32_t info_level_threshold;
extern int32_t info_level_current;

int infof(const char *fmt, ...) {
    /* Bypass everything if not enabled. */
    if (info_level_threshold > info_level_current) return 0;
    va_list ap;
    va_start(ap, fmt);
    int rv = info_vf(fmt, ap);
    va_end(ap);
    return rv;
}

