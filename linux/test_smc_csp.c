#include <stdint.h>
#include "macros.h"
// Context
#define SM_READ(s, chan) ({ return; 123; })
#define add(a,b) ((a)+(b))
typedef uint32_t T;

void send(uint32_t val) {}

// Generated by test_scm.lua from test_csp.sm
// See also test_smc_csp.conf
#include "test_csp.sm.c"

int main(int argc, char **argv) {
    struct state s = {};
    testmod(&s);
    return 0;
}
