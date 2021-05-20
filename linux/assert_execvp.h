#ifndef ASSERT_EXECVP_H
#define ASSERT_EXECVP_H
#include "macros.h"
#include "os_linux.h"

static void assert_execvp(int *in_fd, int *out_fd, const char **argv,
                          void (*handle_sigchld)(int sig)) {

    /* In/out naming is confusing, so use x_to_y naming. */
    int ignore;
    const int read_end = 0;
    const int write_end = 1;
    int parent_to_child[2];
    int child_to_parent[2];
    ASSERT_ERRNO(pipe(parent_to_child));
    ASSERT_ERRNO(pipe(child_to_parent));

    int pid = fork();

    /* CHILD */
    if (!pid){
        /* replace stdio with pipes and leave stderr as-is. */
        close(0); ignore = dup(parent_to_child[read_end]);
        close(1); ignore = dup(child_to_parent[write_end]);
        (void)ignore;

        /* No longer needed */
        close(parent_to_child[read_end]);
        close(parent_to_child[write_end]);
        close(child_to_parent[read_end]);
        close(child_to_parent[write_end]);

        /* execvp requires NULL-terminated array
           FIXME: only supporting single command, no args */
        ASSERT(argv);
        ASSERT(argv[0]);
        ASSERT_ERRNO(execvp(argv[0], (char **)&argv[0]));
        /* not reached (exec success or assert error exit) */
    }

    if (handle_sigchld) {
        signal(SIGCHLD, handle_sigchld);
    }

    /* PARENT */
    *in_fd  = child_to_parent[read_end];  close(child_to_parent[write_end]);
    *out_fd = parent_to_child[write_end]; close(parent_to_child[read_end]);
}

#endif
