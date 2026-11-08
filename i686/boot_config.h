#ifndef BOOT_CONFIG_H
#define BOOT_CONFIG_H

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

    uint8_t ip[4];
    uint32_t app;
    uint32_t top;
    uint32_t plugin;
    uint32_t plugin_load;
    uint8_t netmask[4];
    uint8_t gateway[4];
    uint8_t update[4];
};

#if defined(__i386__)
/* Don't define this on 64 bit arch to avoid the pitfall. */
#include "ethernet.h"
struct boot_config {
    void (*entry)(void);  // kernel entry point
    void* endx;           // end of config + code + initialized data
    void* start;          // start of kernel, i.e. boot_config, always 0x7E00
    struct ip_addr ip;    // default ip addr
    void *app;            // main application static data structure
    void *top;            // free memory starts here, usable by 3if monitor
    void *plugin;         // struct plugin *, see plugin.h
    void *plugin_load;    // struct plugin_header *, loader doorbell
    struct ip_addr netmask;
    struct ip_addr gateway;  // 0: none
    struct ip_addr update;   // update_daemon.sh host, see `update`
};
extern struct boot_config boot_config;
CT_ASSERT(boot_config,sizeof(struct boot_config)==sizeof(struct boot_config_u32));
#endif


#endif
