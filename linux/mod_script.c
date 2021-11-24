/* Forth-style command interpreter.

   This was originally written as a "tethered Forth" interpreter,
   where the threading interpreter runs on a Linux host, and the
   primitives are executed on a collection of microcontroller targets
   by sending primitive commands over a communication bus.

   The Forth-like language is built from 3 kinds of commands:

   1. TARGET COMMANDS

   We access these through interpret_primitive_command().  This can be
   implemented by sending an RPC message to a specific target
   processor.  See e.g. the uc_tools Forth-style target console.  It
   can map strings to functions that operate on the machine state and
   a parameters stack, and can load parameters (typically just
   numbers) onto this stack.

   2. SCRIPTS

   In the uc_tools console case, the primitive set is not a complete
   Forth interpreter.  We implement that on the host.

   The environment variable SCRIPT_DIR points to a directory
   containing scripts, where each script is a file containing
   whitespace separated commands in the same syntax as used by the
   target console.

   3. HOST COMMANDS

   In addition to primitive target commands and composite scripts, we
   implement commands that execute on the host.  In order to make this
   mesh properly with the target commands, the host commands typically
   use the target's parameter stack, such that conceptuall they look
   like a remte call from the target to the host while focus is on a
   particular target.

   Host commands can act as "parsing words", which are commands that
   modify the meaning of the next word, such as filenames, by reading
   ahead in the stream.

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
struct interpreter;
typedef int (*command_fun)(struct interpreter *env);

struct command {
    const char *name;
    command_fun fun;
};
#ifndef INTERP_MAX_WORD_SIZE
#define INTERP_MAX_WORD_SIZE 1024
#endif
struct return_stack;
struct return_stack {
    FILE *thread;
    char *name; // for debugging
    struct return_stack *next;
};
struct interpreter {
    /* Return stack. */
    struct return_stack *return_stack;
    /* Set before accepting the next word,
       e.g. for implementing loops. */
    long here;
    /* Current word */
    char word[INTERP_MAX_WORD_SIZE];
};


/* User provides primitive interpretation (which e.g. could do RPC),
   and a list of commands implemented as C functions running
   locally with access to the string stack (args). */

/* Primitive target command. */
int interpret_primitive_command(struct interpreter *env, const char *cmd);

/* Primitive host command. */
extern struct command commands[];

/* We provide interpreter on top of that + file scripts. */
int interpret_command(struct interpreter *env, const char *cmd);


void interpret_enter(struct interpreter *env, FILE *f, const char *name) {
    //LOG("enter: %s\n", name);
    struct return_stack *frame = malloc(sizeof(*frame));
    ASSERT(frame);
    frame->next = env->return_stack;
    frame->thread = f;
    frame->name = NULL;
    if (name) { asprintf(&frame->name, "%s", name); }
    env->return_stack = frame;
}
void interpret_leave(struct interpreter *env) {
    ASSERT(env->return_stack);
    struct return_stack *frame = env->return_stack;
    //LOG("leave: %s\n", frame->name);
    if (frame->thread) { fclose(frame->thread); }
    if (frame->name) { free(frame->name); }
    env->return_stack = frame->next;
    free(frame);
}
const struct command *find_command(const char *name, const struct command *commands) {
    for (const struct command *c = commands; c->name; c++) {
        if (!strcmp(c->name, name)) { return c; }
    }
    return NULL;
}

int interpret_host_command(struct interpreter *env, const char *cmd) {
    const struct command *c = find_command(cmd, commands);
    if (!c) return 0;
    c->fun(env);
    return 1;
}
int accept_word(struct interpreter *env) {
    int nb_chars = 0;
    int done = 0;
    ASSERT(env->return_stack);
    FILE *ip = env->return_stack->thread;
    env->here = ftell(ip);
    for(;;) {
        /* Read next character. */
        int c = fgetc(ip);
        // LOG("read '%c' %d\n", c, c);
        if(('\\' == c) || (EOF == c) || isspace(c)) {
            /* Found a word delimiter. */
            if (nb_chars > 0) {
                env->word[nb_chars] = 0;
                done = 1;
            }
            /* If there is a comment trailing the word, skip it. */
            if ('\\' == c) {
                do { c = fgetc(ip); }
                while ((EOF != c) && ('\n' != c));
            }
        }
        else {
            /* Keep collecting characters. */
            env->word[nb_chars++] = c;
            ASSERT(nb_chars < sizeof(env->word));
        }
        if (done) {
            return nb_chars;
        }
        if (EOF == c) {
            ASSERT(nb_chars == 0);
            return 0;
        }
    }
}

/* Composite host command script.  Note that we just "enter" the
   script by pushing a new file to the thread stack... */
int interpret_script_command(struct interpreter *env, const char *cmd) {
    const char *dir = getenv("SCRIPT_DIR");
    if (!dir) return 0;
    char *file = NULL;
    asprintf(&file, "%s/%s", dir, cmd);
    ASSERT(file);
    FILE *f = fopen(file, "r");
    free(file);
    if (!f) return 0;
    interpret_enter(env, f, cmd);
    return 1;
}
/* ... the popping of the thread stack happens in the interpreter. */
void interpret_loop(struct interpreter *env) {
    for(;;) {
        int nb_chars = accept_word(env);
        if (nb_chars) {
            interpret_command(env, env->word);
        }
        else {
            /* We've run off the end of a thread file.  Close the
               file, pop the stack, and and continue if there is still
               thread left.  */
            ASSERT(env->return_stack);
            interpret_leave(env);
            if (!env->return_stack) return;
        }
    }
}

int interpret_command(struct interpreter *env, const char *cmd) {
    // LOG("interpret '%s'\n", cmd);
    return
        interpret_host_command(env, cmd) ||
        interpret_script_command(env, cmd) ||
        interpret_primitive_command(env, cmd);
}


/* These are entry points, e.g. they switch between "C mode" which
   uses recursion, and "Forth mode", which uses a threaded interpreter
   loop and an explicit return stack.  File is closed in the process. */
void interpret_file(struct interpreter *env, FILE *f, const char *cmd) {
    /* To support C and Forth nesting, we will need to preserve the
       current return stack. */
    struct return_stack *save = env->return_stack;
    env->return_stack = NULL;
    interpret_enter(env, f, cmd);
    interpret_loop(env);
    env->return_stack = save;
}
void interpret_commands(struct interpreter *env, const uint8_t *bytes, size_t len) {
    FILE *f = fmemopen((void*)bytes, len, "r");
    ASSERT(f);
    interpret_file(env, f, "<console>");
}

#endif

