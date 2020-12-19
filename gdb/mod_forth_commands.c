#ifndef MOD_FORTH_COMMANDS_C
#define MOD_FORTH_COMMANDS_C

#include "command.h"
#define PUSH command_stack_push
#define POP  command_stack_pop

/* These have non-C names. */
void forth_add(void)   { PUSH(POP() + POP()); }                  COMMAND_REGISTER_NAMED("+", forth_add);
void forth_sub(void)   { uint32_t v = POP(); PUSH(POP() - v); }  COMMAND_REGISTER_NAMED("-", forth_sub);
void forth_print(void) { infof("%x\n", POP()); }                 COMMAND_REGISTER_NAMED("p", forth_print);

/* Those that have C names are simpler to abstract. */
DEF_COMMAND(words) { FOR_COMMAND(c) { infof("%s ",(*c)->name); }; infof("\n"); }

#undef PUSH
#undef POP


#endif
