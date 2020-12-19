#ifndef MOD_FORTH_COMMANDS_C
#define MOD_FORTH_COMMANDS_C

#include "command.h"
#define PUSH command_stack_push
#define POP  command_stack_pop

void forth_add(void)   { PUSH(POP() + POP()); }                  DEF_COMMAND_NAMED("+", forth_add);
void forth_sub(void)   { uint32_t v = POP(); PUSH(POP() - v); }  DEF_COMMAND_NAMED("-", forth_sub);
void forth_print(void) { infof("%x\n", POP()); }                 DEF_COMMAND_NAMED("p", forth_print);

#undef PUSH
#undef POP


#endif
