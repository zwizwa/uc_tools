/* Trampoline to start one of two firmware partitions, or none at all
   in case no valid signatures are found.

   This is essentially a bootloader extension and will never be
   updated in the field, so keep it small in footprint and simple so
   it is obviously correct.
*/


/* About partition images.

   - Memory layout.

     The layout is specialized for the 128k Flash STM32F103, which has
     Flash mapped from 0800 0000 to 0802 0000.

                 | config    | firmware
     ------------+-----------+----------
     gdbstub     |           | 0800 0000
     trampoline  | 0800 2800 | 0800 3000  (this file)
     partition a | 0800 4000 | 0800 4800
     partition b | 0801 2000 | 0801 2800

   - This is a secondary loader.  While we re-use the datastructures
     used by the gdbstub loader for convenience, the first stage
     loader does not know about partition a,b or CRC checks.  It will
     merely load this trampoline, which then dispatches to the a,b
     slots.

   - The C API for defining the gdbstub_config block header is the
     same for a main application slot, or for the a,b partitions.
     Location is determined purely by the linker script.

   - The trampoline expects a control block to be appended to the end
     of the firmware.  At this point, it only contains the CRC, but at
     a later point it would be possible to add extra boot
     configuration data that is not included in the firmware binary.
     E.g. to temporarily disable an image.  This control block is at a
     Flash erase boundary so can be updated separately.
*/

/* Platform-independent partition validation routine. */
#include "partition_config.h"

#include "base.h"
#include <string.h>
#include "tools.h"

/* Partition tables are defined in the main bootloader files via
   PARTITION_CONFIG_INIT macro.  Assume for now there are always
   exactly 2 partitions.  They are linked into the trampolines
   gdbstub_config. */
#ifndef PARTITION_CONFIG_INIT
#define PARTITION_CONFIG_INIT PARTITION_CONFIG_DEFAULT_INIT
#endif

static const struct partition_config part[2] = PARTITION_CONFIG_INIT;

/* Indirection to allow future size change of the struct.  This array
   is NULL-terminated and linked to the gdbstub_config data field. */
const struct {
    const struct partition_config *partition_a;
    const struct partition_config *partition_b;
    const struct partition_config *end_marker;
} gdbstub_partition_config = { &part[0], &part[1], (const void*)NULL };


/* Instantiate these functions so they won't be inlined, making them
   available for manual interaction in gdb. */
uint32_t __attribute__ ((noinline)) crc(const uint8_t *buf, uint32_t len) {
    return crc32b(buf, len);
}
const struct gdbstub_control __attribute__ ((noinline)) *valid_partition(const struct partition_config *p) {
    return partition_config_valid(p, crc, NULL);
}

const struct partition_config *choose_partition(
    const struct partition_config *a, const struct partition_config *b) {

    const struct gdbstub_control *a_valid = valid_partition(a);
    const struct gdbstub_control *b_valid = valid_partition(b);

    /* Choice:
       1. If priority is decisive, use that.
       2. Use version string. */
    if (a_valid && b_valid) {
        if (a_valid->priority > b_valid->priority) return a;
        if (b_valid->priority > a_valid->priority) return b;
        return (mini_strcmp(a->config->version,
                            b->config->version) >= 0) ? a : b;
    }
    /* No choice. */
    if (a_valid) { return a; }
    if (b_valid) { return b; }
    /* No valid firmware. */
    return 0;
}


/* Provide wrappers for switch_protocol() and start(). */

void switch_protocol(const uint8_t *buf, uint32_t len) {
    /* Implementation quirk: this function is called after the static
       global variables have been initialized for the application's
       start() function. We are not allowed to use any static memory
       in this function.  I.e. it is essential that part is static
       const, so it is guaranteed to be located in our Flash
       segment. */
    const struct partition_config *p = choose_partition(&part[0], &part[1]);
    if (p && p->config->switch_protocol) {
        p->config->switch_protocol(buf, len);
        return;
    }
}

void trampoline_start(void) {
    const struct partition_config *p = choose_partition(&part[0], &part[1]);
    if (p) p->config->start();
}

void trampoline_app_init_and_start(void) {
    /* Low level application init.  This needs to be called manually
       after loading to initialize memory.  Note that this will be
       immediately undone by the application's start() function, if
       there is one. */
    hw_app_init();

    /* The rest is generic and later can go into the library. */
    trampoline_start();
}

void main_loop(gdbstub_fn_poll bl_poll_fn) {
    const struct partition_config *p = choose_partition(&part[0], &part[1]);
    if (p && p->config->loop) p->config->loop(bl_poll_fn);
}


#ifndef MANUFACTURER
#define MANUFACTURER "Zwizwa"
#endif

#ifndef PRODUCT
#define PRODUCT "Trampoline"
#endif


/* Note that config variables of the trampoline are never updated,
   e.g. the USB strings do not refer to one of the two firmware
   images. */
const char config_manufacturer[] CONFIG_DATA_SECTION = MANUFACTURER;
const char config_product[]      CONFIG_DATA_SECTION = PRODUCT;
const char config_firmware[]     CONFIG_DATA_SECTION = FIRMWARE;

/* This is now generated at link time. */
extern const char config_version[];

struct gdbstub_config trampoline_config CONFIG_HEADER_SECTION = {
    .manufacturer    = config_manufacturer,
    .product         = config_product,
    .firmware        = config_firmware,
    .version         = config_version,
#ifdef TRAMPOLINE_IN_BOOTLOADER
    .start           = trampoline_start,
#else
    .start           = trampoline_app_init_and_start,
#endif
    .switch_protocol = switch_protocol,
    .loop            = main_loop,
    .data            = (void*)&gdbstub_partition_config,
};




/* How to debug.

   This code is very minimal and has no logging.  Use a SWD debugger.

   Here's a note about some issues I ran into trying to properly trap
   start().  It appears that upon reset, gdb/openocd/stlink in my
   current setup cannot trap fast enough.  I do faintly recall this
   working before, but anyways..

   If it doesn't trap and the code also doesn't load any partition, it
   is possible to run "p config.start()" with the trampoline elf
   symbols loaded into gdb.  This will then trap and makes it possible
   to call individual functions to debug why a partition isn't
   loading, e.g. "p valid_partition(&part[0])"

*/
