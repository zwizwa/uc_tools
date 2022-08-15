/* Stand-alone tether for uc_tools/gdb firmware.

   Idea is to replace the erl_tools stubhub with something that

   1. has no dependencies so it is easier to integrate into other
      projects (verified by copying into "net" monorepo),

   2. can be used for non-changing, stable infrastructure firmware
      (e.g. lab_board.c)

   Difference compared to gdbstub_connect.c is that this will get
   started as a local systemd service, connect to the TTY port, and
   expose the multiplexed protocols as one or more TCP ports.
*/

// FIXME: This was part of a chain of events that eventually lead to a
// different solution.  Currently packet_bridge is used to expose the
// serial port over USB in a generic way.  Later this dedicated tether
// might be useful for the forth_dsl or for other dedicated
// applications, but for now I'm back to using the Erlang code
// (original bug that triggered this new Erlang-free approach was a
// hardware issue on 'r' board and is isolated).  Leaving this stub
// here in case inspiration hits.  Thinking about re-doing the 3if
// with a simpler interface that definitely needs a dedicated tether.

#include "macros.h"

int main(int argc, char **argv) {
    ASSERT(argc == 2);
    const char *tty = argv[1];
    const char *log_port = getenv("LOG_PORT");
    for(;;) {
        LOG("tether.c %s\n", tty);
        sleep(1);
    }
    (void)log_port;
    return 0;
}
