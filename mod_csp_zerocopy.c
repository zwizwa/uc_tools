#ifndef MOD_CSP_ZEROCOPY
#define MOD_CSP_ZEROCOPY

/* Specialize csp.c for synchronization only, where sender provides a
   csp_data (pointer or integer), and the receiver needs to use that
   before suspending again. */
#define CSP_CONF_COPY 0
#include "csp.h"
#include "csp.c"

#endif
