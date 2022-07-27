//$ nix-shell -p readline --command 'gcc test_readline.c -lreadline -o test_readline.elf'

/* Simple readline example with tab completer. */

#define _GNU_SOURCE 1
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <readline/readline.h>
#include <readline/history.h>

#include "macros.h"

/* text: partial word for which we generate completions.
   state: zero on first call, and positive nonzero on subsequent calls. */
static char *name_generator(const char *text, int state) {
    static int count;
    if (!state) { count = 0; }
    if (count > 3) { return NULL; }
    char *compl = NULL;
    ASSERT(-1 != asprintf(&compl, "%s%d-%d", text, count++, state));
    return compl;
}

static char **completion(const char *text, int start, int end) {
    rl_attempted_completion_over = 1;
    return rl_completion_matches(text, name_generator);
}

int main(int argc, char** argv) {
    rl_attempted_completion_function = completion;
    char* buf;
    while ((buf = readline(">> ")) != NULL) {
        if (strlen(buf) > 0) {
            add_history(buf);
        }
        printf("[%s]\n", buf);
        free(buf);
    }
    return 0;
}
