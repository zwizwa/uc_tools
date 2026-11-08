/* Kernel <-> plugin interface.
   - plugin is a flat binary at PLUGIN_ADDR, linked with
     ld -R kernel.elf against the running kernel, see plugin.ld
   - image starts with struct plugin_header, data only
   - `tether_udp plugin` clears boot_config.plugin, writes the image,
     then sets boot_config.plugin_load to the header address: fixed
     addresses, so this works whatever kernel runs.  No 3if JSR.
   - the net isr turns plugin_load into KERNEL_EVENT_PLUGIN, the main
     loop checks magic and kernel_version, clears bss, calls init and
     registers what it returns.  No plugin code runs in an isr.
   - no ABI guarantees beyond the header */

#ifndef PLUGIN_H
#define PLUGIN_H

#include <stdint.h>
#include "blockdev.h"

#define PLUGIN_ADDR 0x2000000  // 32MB, above stack and 3if kernel image

/* Kernel events, queued by the isrs, delivered by the main loop */
#define KERNEL_EVENT_LINE   0  // console line incl '\n', default: forth
#define KERNEL_EVENT_PLUGIN 1  // uint32_t header address, default: load
#define KERNEL_EVENT_INPUT  2  // console bytes, default: telnet, which
                               // makes KERNEL_EVENT_LINE in the main loop
#define KERNEL_EVENT_LOG    3  // LOG text from an isr, default: print

/* A plugin is a state machine driven by the events it listens to. */
struct plugin {
    const char *name;
    uint32_t events;  // mask of 1 << KERNEL_EVENT_*
    /* Main loop, interrupts on.  Returns 0 when handled, else the
       kernel's default handler gets it. */
    int (*event)(uint32_t tag, const uint8_t *data, uint32_t len);
};

#define PLUGIN_MAGIC 0x6e67706c  // "lpgn"
struct plugin_header {
    uint32_t magic;
    char kernel_version[16];  // KERNEL_VERSION at build time
    uint8_t *bss_start, *bss_end;
    struct plugin *(*init)(void);  // main loop, returns what to register
};

/* Provided by the kernel */
extern struct blockdev *kernel_blockdev;  // type 7f partition, or 0
extern const char kernel_version[];
int kernel_infof(const char *fmt, ...);

#endif
