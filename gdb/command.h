#ifndef COMMAND_H
#define COMMAND_H

/* Note that this requires linker script support to create the global
   command array. */

/* Contains pointers to init function for static commands. */
#define COMMAND_SECTION      __attribute__ ((section (".command")))

/* The command section is only an index, i.e. it collects only
   pointers to a command struct.  This leaves some freedom to the
   application about how to implement commands.
*/
struct command {
    const char *name;
    void (*run)(void);
};
extern const struct command const *_command_start;
extern const struct command const *_command_endx;

#define FOR_COMMAND(c) \
    for (const struct command **c = &_command_start; c < &_command_endx; c++)
#define DEF_COMMAND_NAMED(_name, _cname) \
    const struct command command_##_cname = { .name = _name, .run = _cname }; \
    const struct command *command_ref_##_cname COMMAND_SECTION = &command_##_cname

#define DEF_COMMAND(_cname) DEF_COMMAND_NAMED(#_cname, _cname)

/* In uc_tools, the command interpreter is set up as a Forth-style RPN
   machine to simplify the glue code.  However this requires that the
   functions that implement the command know about the argument
   stack. */
uint32_t command_stack_pop(void);
void     command_stack_push(uint32_t);

static inline void command_handle(const char *cmd_buf) {

    /* Run it if it's in the dictionary. */
    FOR_COMMAND(c) {
        if (!strcmp(cmd_buf, (*c)->name)) {
            (*c)->run();
            return;
        }
    }
    /* Otherwise, convert to number if it parses. */
    uintptr_t word = 0;
    if (0 == read_hex_nibbles_check_uptr(
            (const uint8_t*)cmd_buf, strlen(cmd_buf), &word)) {
        command_stack_push(word);
        return;
    }

    /* Otherwise complain. */
    LOG("?\n");
}

#endif
