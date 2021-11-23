/* Forth-style command interpreter, unifying outer intepreter (parsing
   words) and inner interpreter.

   This is not very efficient and was originally written as a
   scripting language for primitives implemented using a remote
   procedure call mechanism.

   While this is fairly general (primitives are abstract), the
   comments use the concept of 'target', a machine that is different
   from the host machine which is what runs this interpreter,
   implements the host words, and has a file system that stores the
   scripts.


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
struct interpreter {
    /* Current word stream. */
    FILE *thread;
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
    env->here = ftell(env->thread);
    for(;;) {
        /* Read next character. */
        int c = fgetc(env->thread);
        // LOG("read '%c' %d\n", c, c);
        if(('\\' == c) || (EOF == c) || isspace(c)) {
            /* Found a word delimiter. */
            if (nb_chars > 0) {
                env->word[nb_chars] = 0;
                done = 1;
            }
            /* If there is a comment trailing the word, skip it. */
            if ('\\' == c) {
                do { c = fgetc(env->thread); }
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
void interpret_file(struct interpreter *env, FILE *f) {
    int nb_chars;
    /* push */
    FILE *prev = env->thread; env->thread = f;
    /* interpret thread */
    while ((nb_chars = accept_word(env))) {
        // LOG("accepted '%s'\n", word);
        interpret_command(env, env->word);
    }
    /* pop */
    env->thread = prev;
}
/* Composite host command script. */
int interpret_script_command(struct interpreter *env, const char *cmd) {
    const char *dir = getenv("SCRIPT_DIR");
    if (!dir) return 0;
    char *file = NULL;
    asprintf(&file, "%s/%s", dir, cmd);
    ASSERT(file);
    FILE *f = fopen(file, "r");
    free(file);
    if (!f) return 0;
    // LOG("host_command '%s'\n", file);
    interpret_file(env, f);
    fclose(f);
    return 1;
}
int interpret_command(struct interpreter *env, const char *cmd) {
    // LOG("interpret '%s'\n", cmd);
    return
        interpret_host_command(env, cmd) ||
        interpret_script_command(env, cmd) ||
        interpret_primitive_command(env, cmd);
}
void interpret_commands(struct interpreter *env, const uint8_t *bytes, size_t len) {
    FILE *f = fmemopen((void*)bytes, len, "r");
    ASSERT(f);
    interpret_file(env, f);
    fclose(f);
}

#endif

