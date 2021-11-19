/* Forth-style command interpreter with "parsing" words and file-based
   dictionary.

   1. TARGET COMMANDS

   This is a composition mechanism based around a primitive stack
   language defined elsewhere.  We access it here through
   interpret_primitive_command().

   Typically is a "target console", a Forth-style command interpreter
   on a microcontroller, which looks up commands in a dictionary and
   executes the associated function, and can push numbers to a stack.
   The command functions can then get parameters from that stack.

   2. SCRIPTS

   The primitive set is not a complete Forth interpreter.  We assume
   there is no mechanism to compose new commands based out of other
   commands.  We implement that here.

   The environment variable SCRIPT_DIR points to a directory
   containing scripts, where each script is a file containing
   whitespace separated commands in the same syntax as used by the
   target console.

   3. HOST COMMANDS

   In addition to primitive target commands and composite scripts, we
   implement commands that execute on the side that runs the the
   interpreter.  In order to make this mesh properly with the target
   commands, the host commands also use the target's parameter stack.
   Think of them as an RPC call from the target to the host, that are
   accessible while being connected to a particular target.

   This also implements so called "parsing words", which are commands
   that modify the meaning of the next word, such as filenames.

*/


#ifndef MOD_SCRIPT
#define MOD_SCRIPT

#include "macros.h"
#include <stdint.h>
#include <ctype.h>

/* A Forth-like scripting language. */
typedef const char * arg;
struct args {
    char *arg;
    struct args *next;
};
struct interp;
typedef int (*command_fun)(struct interp *env);

struct command {
    const char *name;
    command_fun fun;
    int nb_args;
};
struct interp {
    /* String stack for parsing commands. */
    const struct command *command;
    struct args *args;
    int nb_args;
};


/* We implement "parsing words" by pushing strings to a separate
   stack.  Once all arguments are collected the command is
   executed. */
int push_arg(struct interp *env, const char *cmd) {
    int rv = -1;
    ASSERT(env->command);
    struct args *a = malloc(sizeof(*a));
    a->arg = malloc(1 + strlen(cmd));
    strcpy(a->arg, cmd);
    a->next = env->args;
    env->args = a;
    env->nb_args++;
    if (env->nb_args == env->command->nb_args) {
        rv = env->command->fun(env);
        while(env->args) {
            struct args *a = env->args;
            env->args = a->next;
            free(a->arg);
            free(a);
        }
        env->command = NULL;
        env->nb_args = 0;
    }
    return rv;
}

const char *get_arg(struct interp *env, int n) {
    ASSERT(n >= 0);
    struct args *a = env->args;
    while(n > 0) {
        ASSERT(a);
        a = a->next;
    }
    ASSERT(a);
    return a->arg;
}

const struct command *find_command(const char *name, const struct command *commands) {
    for (const struct command *c = commands; c->name; c++) {
        if (!strcmp(c->name, name)) { return c; }
    }
    return NULL;
}

void interpret(struct interp *env, uint8_t *bytes, size_t len);

/* Primitive target command. */
int interpret_primitive_command(struct interp *env, const char *cmd);

/* Primitive host command. */
extern struct command commands[];
int interpret_host_command(struct interp *env, const char *cmd) {
    /* This is mode-dependent */
    if (env->command) {
        /* A parsing command is active.  Push string arguments to a
           stack and execute the command when all string arguments are
           in. */
        push_arg(env, cmd);
        return 1;
    }
    else {
        /* No parsing command is active, look it up. */
        const struct command *c = find_command(cmd, commands);
        if (!c) return 0;
        if (c->nb_args == 0) {
            /* Not a parsing command. Can be executed right away. */
            c->fun(env);
        }
        else {
            /* If this is a parsing command, we need to save the
               command info and wait for more string arguments. */
            env->command = c;
        }
        return 1;
    }
}
/* Composite host command script. */
int interpret_script_command(struct interp *env, const char *cmd) {
    const char *dir = getenv("SCRIPT_DIR");
    if (!dir) return 0;
    int len = strlen(cmd);
    int dir_len = strlen(dir);
    char file[dir_len + 1 + len + 1];
    strcpy(file, dir);
    file[dir_len] = '/';
    strcpy(file + dir_len + 1, cmd);
    FILE *f = fopen(file, "r");
    if (!f) return 0;
    fseek(f, 0, SEEK_END);
    long bufsize = ftell(f);
    uint8_t buf[bufsize];
    fseek(f, 0, SEEK_SET);  /* same as rewind(f); */
    fread(buf, 1, bufsize, f);
    fclose(f);
    // LOG("host_command: %s\n", file);
    interpret(env, buf, bufsize);
    return 1;
}
int interpret_command(struct interp *env, const char *cmd) {
    return
        interpret_host_command(env, cmd) ||
        interpret_script_command(env, cmd) ||
        interpret_primitive_command(env, cmd);
}
void interpret_command_slice(struct interp *env, const uint8_t *bytes, size_t len) {
    char cmd[len+1];
    memcpy(cmd, bytes, len);
    cmd[len] = 0;
    interpret_command(env, cmd);
}

// FIXME: Implement comments
void interpret(struct interp *env, uint8_t *bytes, size_t len) {
    uint8_t *word_start = bytes;
    uint8_t nb_chars = 0;

  next:
    if (word_start + nb_chars >= bytes + len) {
        /* Delimited by end. */
        if (nb_chars > 0) { goto call;  }
        return;
    }
    uint8_t c = word_start[nb_chars];
    if(isspace(c)){
        /* Delimited by space. */
        if (nb_chars > 0) { goto call; }
        else { word_start++; }
    }
    else {
        /* Keep collecting characters. */
        nb_chars++;
    }
    goto next;

  call:
    interpret_command_slice(env, word_start, nb_chars);
    word_start += nb_chars;
    nb_chars = 0;
    goto next;
}

/* Wrapper around 1-argument arg_command */
int call_1(struct interp *env, const char *cmd, const char *arg) {
    ASSERT(!env->command);
    env->command = find_command(cmd, commands);
    ASSERT(env->command);
    return push_arg(env, arg);
}


#endif

