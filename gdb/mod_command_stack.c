#ifndef MOD_COMMAND_STACK_C
#define MOD_COMMAND_STACK_C

/* Circular argument stack. */
#ifndef COMMAND_STACK_LOGSIZE
#define COMMAND_STACK_LOGSIZE 3
#endif
#define COMMAND_STACK_MASK ((1<<COMMAND_STACK_LOGSIZE)-1)
uint32_t command_stack[COMMAND_STACK_MASK+1];
uint32_t command_stack_top;
void command_stack_push(uint32_t word) {
    command_stack[(--command_stack_top) & COMMAND_STACK_MASK] = word;
}
uint32_t command_stack_pop(void) {
    return command_stack[(command_stack_top++) & COMMAND_STACK_MASK];
}



#endif
