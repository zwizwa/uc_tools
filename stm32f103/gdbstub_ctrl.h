#ifndef GDBSTUB_CTRL_H
#define GDBSTUB_CTRL_H

#include <stdint.h>

#define GDBSTUB_FLAG_STARTED (1 << 0)
#define GDBSTUB_FLAG_LOOP    (1 << 1)

// Core functionality, needed even if gdbstub is not defined.
struct gdbstub_ctrl {
    uint32_t flags;
};

void ensure_started(struct gdbstub_ctrl *stub_ctrl);


#endif

