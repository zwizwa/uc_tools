#ifndef COMMAND_H
#define COMMAND_H

#include "log.h"
#include "tools.h"
#include <string.h>

/* Note that this requires linker script support to create the global
   command array. */

/* Contains pointers to init function for static commands. */
#define COMMAND_SECTION      __attribute__ ((section (".command")))

/* The command section is only an index, i.e. it collects only
   pointers to a command struct.  This leaves some freedom to the
   application about how to implement commands.
*/
typedef void (*command_fn)(void);
struct command {
    const char *name;
    command_fn run;
};
extern const struct command * const _command_start;
extern const struct command * const _command_endx;

static inline uintptr_t command_index(const struct command * const* cmd) {
    return cmd - &_command_start;
}
static inline const struct command * const* command_ref(uintptr_t index) {
    return &_command_start + index;
}
static inline uintptr_t command_index_size(void) {
    return &_command_endx - &_command_start;
}

#define FOR_COMMAND(c) \
    FOR_START_ENDX(&_command_start, &_command_endx, c)

#define COMMAND_REGISTER_NAMED(_name, _cname) \
    const struct command command_##_cname = { .name = _name, .run = (command_fn)_cname }; \
    const struct command *command_ref_##_cname COMMAND_SECTION = &command_##_cname

#define COMMAND_REGISTER(_cname) \
    COMMAND_REGISTER_NAMED(#_cname, _cname)

#define DEF_COMMAND_NAMED(_name, _cname) \
    void _cname(void); \
    COMMAND_REGISTER_NAMED(_name, _cname); \
    void _cname(void)

/* If a dedicated function needs to be implemented, and the command
   name is a C name, then this shorthand can be used. */
#define DEF_COMMAND(_name) \
    DEF_COMMAND_NAMED(#_name, command_fun_##_name)


/* In uc_tools, the command interpreter is set up as a Forth-style RPN
   machine to simplify the glue code.  However this requires that the
   functions that implement the command know about the argument
   stack. */
uintptr_t command_stack_pop(void);
void      command_stack_push(uintptr_t);

/* RV: 0 is ok, anything else is error code. */
static inline uintptr_t command_handle_base(const char *cmd_buf, uintptr_t base) {

    /* Otherwise, convert to number if it parses. */
    uintptr_t word = 0;
    uint32_t len = strlen(cmd_buf);
    if ((len >= 3) && ('0' == cmd_buf[0]) && ('x' == cmd_buf[1])) {
        if (0 == read_hex_nibbles_check_uptr(
                (const uint8_t*)cmd_buf + 2, len-2, &word)) {
            command_stack_push(word);
            return 0;
        }
        goto bad_number;
    }
    if (10 == base) {
        if (0 == read_dec_nibbles_check_uptr(
                (const uint8_t*)cmd_buf, strlen(cmd_buf), &word)) {
            command_stack_push(word);
            return 0;
        }
    }
    else if (16 == base) {
        if (0 == read_hex_nibbles_check_uptr(
                (const uint8_t*)cmd_buf, len, &word)) {
            command_stack_push(word);
            return 0;
        }
        goto bad_number;
    }

  bad_number:
    /* Run it if it's in the dictionary.  Note that in tranditional
     * Forth style, this should happen before parsing numbers, to be
     * able to override the implementation of numbers, which
     * e.g. makes sense for a compiling Forth.  Since this is used
     * here as a protocol, we really do not want a list search miss
     * for each number, so it is done when number parsing fails,
     * meaning that commands that look like numbers are never
     * found. */
    FOR_COMMAND(c) {
        if (!strcmp(cmd_buf, (*c)->name)) {
            (*c)->run();
            return 0;
        }
    }

    /* Undefined */
    return 1;
}

static inline uintptr_t command_handle(const char *cmd_buf) {
    return command_handle_base(cmd_buf, 10);
}

#endif
