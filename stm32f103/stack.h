#ifndef STACK_H
#define STACK_H
#include <stdint.h>
// App shares gdbstub C stack
// Mark the C stack
void mark_stack(void);
// After marking, this returns an estimate of nb_bytes unused.
uintptr_t check_stack(void);
// A bump allocator that allocates from bottom of stack.
void bump_reset(void);
void *bump_alloc(uintptr_t bytes);

#endif
