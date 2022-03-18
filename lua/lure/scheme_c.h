#ifndef LURE_SCHEME_C_H
#define LURE_SCHEME_C_H
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
typedef intptr_t T;
T trace_max = 10;
static inline T trace(T val) {
    printf("%d\n", val);
    if (!trace_max) exit(0);
    trace_max--;
    return val;
}
#endif
