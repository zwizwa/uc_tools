/* Trampoline to start one of two firmware partitions, or none at all
   in case no valid signatures are found.

   This is essentially a bootloader extension and will never be
   updated in the field, so keep it small in footprint and simple so
   it is obviously correct.
*/

#include "base.h"
#include "gdbstub_api.h"
#include <string.h>
#include "crc.h"

struct pconfig {
    const struct gdbstub_config *config;
    uint32_t max_size;
};

static int is_valid_partition(const struct pconfig *p) {
    /* The config struct can be dereferenced as we guarantee it points
       into mapped Flash memory, but it might contain garbage. */
    uint32_t start_addr = (uint32_t)p->config;
    uint32_t endx_addr = p->config->bottom;

    /* Do some consistency checks. */
    if (!endx_addr) return 0;
    if (endx_addr <= start_addr) return 0;
    if (endx_addr > (start_addr + (p->max_size - 4))) return 0;

    /* We now know that the range points into the region we expect.
       Retreive the stored 32-bit CRC appended at the end of the
       image. */
    uint32_t expected_crc = *(uint32_t*)endx_addr;
    uint32_t computed_crc = crc32b((uint8_t*)start_addr, endx_addr - start_addr);
    if (expected_crc != computed_crc) return 0;

    /* We're good. */
    return 1;

}
const struct pconfig *pick_most_recent(
    const struct pconfig *a, const struct pconfig *b) {

    int a_valid = is_valid_partition(a);
    int b_valid = is_valid_partition(b);
    /* Choice, pick the most recent one based on the version string. */
    if (a_valid && b_valid) {
        return (strcmp(a->config->version,
                       b->config->version) >= 0) ? a : b;
    }
    /* No choice. */
    if (a_valid) { return a; }
    if (b_valid) { return b; }
    /* No valid firmware. */
    return 0;
}

/* We are specialized to the partitions provided in the linker files,
   so these are hardcoded. */

/* Provide wrappers for switch_protocol() and start(). */

static const struct pconfig part[] = {
    { .max_size = 0xD000, .config = (void*)0x08004000 },
    { .max_size = 0xD000, .config = (void*)0x08012000 },
};

void switch_protocol(const uint8_t *buf, uint32_t len) {
    const struct pconfig *p = pick_most_recent(&part[0], &part[1]);
    if (p && p->config->switch_protocol) {
        p->config->switch_protocol(buf, len);
        return;
    }
}
void start(void) {
    /* Low level application init.  This needs to be called manually
       after loading to initialize memory. */
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



