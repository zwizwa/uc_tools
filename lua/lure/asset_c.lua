# -*- c -*-
return {
['scheme_c.h'] = [[
#ifndef LURE_SCHEME_C_H
#define LURE_SCHEME_C_H
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
typedef uintptr_t T;
T trace_max = 10;
static inline void trace(T val) {
    printf("%d\n", val);
    if (!trace_max) exit(0);
    trace_max--;
}
#endif
]],
}
