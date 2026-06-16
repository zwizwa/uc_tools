#ifndef BOOT_CONFIG_H
#define BOOT_CONFIG_H

#include "ethernet.h"

/* Like gdbstub_config on ARM.  Let's start over. */

/* Overlay using uint32_t instead of pointers.
   This is for use in the host-side loader application, which has 64
   bit pointers. */
struct boot_config_u32 {
    // These 2 are referenced in trampoline.asm
    uint32_t entry;
    uint32_t endx;
    // Start is always 0x7E00
    uint32_t start;

    struct ip_addr ip;
    uint32_t app;
    uint32_t top;
};

#if defined(__i386__)
/* Don't define this on 64 bit arch to avoid the pitfall. */
struct boot_config {
    void (*entry)(void);  // kernel entry point
    void* endx;           // end of config + code + initialized data
    void* start;          // start of kernel, i.e. boot_config, always 0x7E00
    struct ip_addr ip;    // default ip addr
    void *app;            // main application static data structure
    void *top;            // free memory starts here, usable by 3if monitor
};
extern struct boot_config boot_config;
CT_ASSERT(boot_config,sizeof(struct boot_config)==sizeof(struct boot_config_u32));
#endif


#endif
