#ifndef MOD_EXEC
#define MOD_EXEC

/* Process control wrapper between arbitrary process and
   jack_client.erl tag protocol.

   This essentially replaces the socat wrapper with something more
   flexible, conforming to tag protocol used throughout.
*/

#define EXEC_LOG_CHUNK_SIZE 1000

#include "assert_write.h"
#include "assert_read.h"
#include "macros.h"
#include "uct_byteswap.h"
#include <unistd.h>
#include <poll.h>
#include <signal.h>

int child_pid = -1;
static void cleanup(void) {
    LOG("cleanup\n");
    kill(child_pid, SIGKILL);
}

int exec_main(int argc, char **argv) {
    int pipefd[2]; // {read, write}
    ASSERT_ERRNO(pipe(pipefd));
    ASSERT_ERRNO(child_pid = fork());

    if (0 == child_pid) {
        /* Child */
        close(1); dup(pipefd[1]);
        close(2); dup(pipefd[1]);
        close(pipefd[0]);

        int sub_argc = argc - 1;
        char *sub_argv[sub_argc + 1 /*NULL*/];
        for (int i=0; i<sub_argc; i++) {
            sub_argv[i] = argv[i+1];
        }
        sub_argv[sub_argc] = NULL;

        ASSERT_ERRNO(execvp(sub_argv[0], sub_argv));
    }

    /* Parent */
    atexit(cleanup);
    close(pipefd[1]);
    int logfd = pipefd[0];
    struct pollfd pfd[2] = {
        [0] = { .events = POLLIN, .fd = 0  },
        [1] = { .events = POLLIN, .fd = logfd }
    };
    for(;;) {
        int rv;
        ASSERT_ERRNO(rv = poll(&pfd[0], 2, -1));
        ASSERT(rv >= 0);
        if (pfd[0].revents & POLLIN) {
            /* Handle Erlang command.  Needs to be atomic, so blocking
               read is used here. */
            uint8_t len_buf[4];
            assert_read(0, len_buf, 4);
            uint32_t len = read_be(len_buf, 4);
            uint8_t buf[len];
            assert_read(0, buf, len);
            // FIXME: HANDLE
        }
        else {
            if (0 != pfd[0].revents) goto error;
        }
        if (pfd[1].revents & POLLIN) {
            /* Wrap process output as a log message. */
            uint8_t buf[6 + EXEC_LOG_CHUNK_SIZE];
            int read_rv = read(logfd, buf+6, EXEC_LOG_CHUNK_SIZE);
            ASSERT(read_rv > 0);
            //       dst     value         nb_bytes
            write_be(buf,    2 + read_rv,  4);
            write_be(buf+4,  0xFFFE,       2);
            assert_write(1, buf, 6 + EXEC_LOG_CHUNK_SIZE);
        }
        else {
            if (0 != pfd[1].revents) goto error;
        }
    }
  error:
    exit(1);
    return 0;
}

#endif
