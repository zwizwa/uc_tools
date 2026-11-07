/* Kernel <-> plugin interface.
   - plugin is a flat binary at PLUGIN_ADDR, linked with
     ld -R kernel.elf against the running kernel, see plugin.ld
   - no ABI guarantees, plugin_start checks kernel_version
   - loaded and started by `tether_udp plugin`, from the 3if isr:
     plugin_start only registers, work runs from the main loop
   - registered in boot_config.plugin, at a fixed address so the
     loader can clear it before overwriting the slot, whatever kernel
     runs */

#ifndef PLUGIN_H
#define PLUGIN_H

#include <stdint.h>
#include "blockdev.h"

#define PLUGIN_ADDR 0x2000000  // 32MB, above stack and 3if kernel image

struct plugin {
    const char *name;
    /* Main loop, once per wakeup, interrupts on.  Optional. */
    void (*poll)(void);
    /* Main loop, each command line incl '\n'.  Returns 0 when
       handled, else it goes to forth.  Optional. */
    int (*cmd)(const uint8_t *line, uint32_t len);
};

/* Provided by the kernel */
extern struct blockdev *kernel_blockdev;  // type 7f partition, or 0
extern const char kernel_version[];
int kernel_infof(const char *fmt, ...);

#endif
