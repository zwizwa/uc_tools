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
    /* String stack. */
    struct args *args;
    /* Current string parsing command. */
    const struct command *command;
    int need_args;
};


/* User provides primitive interpretation (which e.g. could do RPC),
   and a list of commands implemented as C functions running
   locally with access to the string stack (args). */

/* Primitive target command. */
int interpret_primitive_command(struct interp *env, const char *cmd);

/* Primitive host command. */
extern struct command commands[];





/* We implement "parsing words" by pushing strings to the string
   stack.  Once all arguments are collected the command is executed.
   The string stack can be used for other things. */
void push_string(struct interp *env, const char *cmd) {
    struct args *a = malloc(sizeof(*a));
    a->arg = malloc(1 + strlen(cmd));
    strcpy(a->arg, cmd);
    a->next = env->args;
    env->args = a;
}
void drop_string(struct interp *env) {
    struct args *a = env->args;
    env->args = a->next;
    free(a->arg);
    free(a);
}
void drop_command(struct interp *env) {
    if (!env->command) return;
    for(int i=0; i<env->command->nb_args; i++) {
        drop_string(env);
    }
    env->command = NULL;
}

int push_arg(struct interp *env, const char *cmd) {
    int rv = -1;
    ASSERT(env->command);
    push_string(env, cmd);
    if (0 == (--env->need_args)) {
        rv = env->command->fun(env);
        drop_command(env);
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

void interpret(struct interp *env, const uint8_t *bytes, size_t len);

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
            env->need_args = c->nb_args;
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
    // LOG("interpret_command: '%s'\n", cmd);
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
void interpret(struct interp *env, const uint8_t *bytes, size_t len) {
    const uint8_t *endx = bytes + len;
    const uint8_t *word_start = bytes;
    uint8_t nb_chars = 0;
    const int end = -1;

    for(;;) {
        /* Read next character. */
        int c = (word_start + nb_chars < endx) ?
            word_start[nb_chars] : end;

        if(('\\' == c) || (end == c) || isspace(c)) {
            /* Found a word delimiter. */
            if (nb_chars > 0) {
                interpret_command_slice(env, word_start, nb_chars);
            }
            /* That was it. */
            if (end == c) {
                return;
            }
            /* Skip past word to start collecting again. */
            word_start += nb_chars + 1;
            nb_chars = 0;
            /* If there was a comment, skip it. */
            if ('\\' == c) {
                do { word_start++; }
                while ((word_start < endx) && word_start[0] != '\n');
            }
        }
        else {
            /* Keep collecting characters. */
            nb_chars++;
        }
    }
}

/* Wrapper around 1-argument arg_command */
int call_1(struct interp *env, const char *cmd, const char *arg) {
    ASSERT(!env->command);
    env->command = find_command(cmd, commands);
    ASSERT(env->command);
    return push_arg(env, arg);
}


#endif

