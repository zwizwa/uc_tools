#ifndef MOD_ABORT_LONGJMP
#define MOD_ABORT_LONGJMP

// Include this before including uc_tools/macros.h
// This will implement ABORT in ERROR and ASSERT messages via setjmp/longjmp.
// THIS IS ONLY FOR TEST CODE

#include <setjmp.h>
jmp_buf abort_jmp_buf;
void abort_longjmp(void) { longjmp(abort_jmp_buf, 1); }
#define ABORT abort_longjmp()

#define CATCH if (setjmp(abort_jmp_buf))

#endif
