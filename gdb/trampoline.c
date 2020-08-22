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

#include "base.h"
#include "gdbstub_api.h"
#include <string.h>
#include "crc.h"
#include "tools.h"

/* Environment used in the functions below. */
struct pconfig {
    const struct gdbstub_config *config;
    uint32_t max_size;
    uint32_t page_size;
};

/* We are specialized to the partitions provided in the linker files,
   so these are hardcoded. */
static const struct pconfig part[] = {
    { .max_size = 0xE000, .page_size = 1<<10, .config = (void*)0x08004000 },
    { .max_size = 0xE000, .page_size = 1<<10, .config = (void*)0x08012000 },
};


static int is_valid_partition(const struct pconfig *p) {
    /* The config struct can be dereferenced as we know it points into
       mapped Flash memory, but it might still contain garbage. */
    const uint8_t *start = p->config->flash_start;
    const uint8_t *endx  = p->config->flash_endx;

    /* Make sure it is loaded into flash at the correct address. */
    if ((void*)start != (void*)p->config) return 0;

    /* Make sure the image is inside the partition boundaries.  One
       page is reserved for the control block. */
    if (endx <= start) return 0;
    if (endx > (start + (p->max_size - p->page_size))) return 0;

    /* We now know that the firmware and the control block are in
       meaningful locations.  Compute and check CRC. */
    const struct gdbstub_control *control = (void*)endx;
    uint32_t computed_crc = crc32b(start, endx-start);
    if (control->crc != computed_crc) return 0;

    /* We're good. */
    return 1;

}

const struct pconfig *pick_most_recent(
    const struct pconfig *a, const struct pconfig *b) {

    int a_valid = is_valid_partition(a);
    int b_valid = is_valid_partition(b);
    /* Choice, pick the most recent one based on the version string. */
    if (a_valid && b_valid) {
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
    const struct pconfig *p = pick_most_recent(&part[0], &part[1]);
    if (p && p->config->switch_protocol) {
        p->config->switch_protocol(buf, len);
        return;
    }
}
void start(void) {
    /* Low level application init.  This needs to be called manually
       after loading to initialize memory.  Note that this will be
       immediately undone by the application's start() function, if
       there is one. */
    hw_app_init();

    /* The rest is generic and later can go into the library. */
    const struct pconfig *p = pick_most_recent(&part[0], &part[1]);
    if (p) p->config->start();
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
const char config_version[]      CONFIG_DATA_SECTION = BUILD;

struct gdbstub_config config CONFIG_HEADER_SECTION = {
    .manufacturer    = config_manufacturer,
    .product         = config_product,
    .firmware        = config_firmware,
    .version         = config_version,
    .start           = start,
    .switch_protocol = switch_protocol,
};




