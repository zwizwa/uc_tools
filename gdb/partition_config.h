#ifndef PARTITION_CONFIG_H
#define PARTITION_CONFIG_H

/* Data structures used by trampoline.c and firmware, to perform
   firmware validation.  This is the part that goes into the
   bootloader, and is kept minimal and should not change over time.
   For code that runs part of the firmware image, see fwstream.h */

#include "crc.h"
#include "gdbstub_api.h"

/* Partition configuration information. */
struct partition_config {
    const struct gdbstub_config *config;
    uint32_t max_size;
    uint32_t page_logsize;
};

/* Logging filled in by partition_config_valid_partition. */
struct partition_config_log {
    uint32_t status;
    uint32_t crc_fw;
    uint32_t crc_ctrl;
};

/* Default for stm32f103 with 2 partitions.
   These are hardcoded in the x8a and x8b linker files. */
#define PARTITION_CONFIG_DEFAULT_PARTITION_INIT(addr) \
  { .max_size = 0xE000, .page_logsize = 10, .config = (void*)(addr) } \

#define PARTITION_CONFIG_DEFAULT_INIT {                     \
        PARTITION_CONFIG_DEFAULT_PARTITION_INIT(0x08004000),        \
        PARTITION_CONFIG_DEFAULT_PARTITION_INIT(0x08012000),        \
}

// TODO: implement status codes.
static inline const struct gdbstub_control *partition_config_valid(
    const struct partition_config *p,
    uint32_t (*crc)(const uint8_t *buf, uint32_t len),
    struct partition_config_log *log) {

    uint32_t page_size = 1 << p->page_logsize;
    /* The config struct can be dereferenced as we know it points into
       mapped Flash memory, but it might still contain garbage. */
    const uint8_t *start = p->config->flash_start;
    const uint8_t *endx  = p->config->flash_endx;

    /* Make sure it is loaded into flash at the correct address. */
    if ((void*)start != (void*)p->config) return 0;

    /* Make sure the image is inside the partition boundaries.  One
       page is reserved for the control block. */
    if (endx <= start) return 0;
    if (endx > (start + (p->max_size - page_size))) return 0;

    /* We now know that the firmware is in a meaningful location.  Get
       the control block pointer and check it points into the
       partition. */
    const struct gdbstub_control *control = p->config->control;
    const uint8_t *ctrl = (void*)control;
    if (ctrl <= endx) return 0;
    if (ctrl > (start + (p->max_size - page_size))) return 0;

    /* The next value we need to trust is the size of the control
       block.  The CRC for the control block is stored after the
       control block.  Check that location of CRC slot is within the
       page reserved for the control block... */
    if (control->size > page_size) return 0;
    /* ... and that the entire struct is accounted for. */
    if (control->size < sizeof(*control)) return 0;

    /* Validate control block. */
    uint32_t computed_ctrl_crc = crc((void*)control, control->size - 4);
    if (log) { log->crc_ctrl = computed_ctrl_crc; }
    if (control->ctrl_crc != computed_ctrl_crc) return 0;

    /* We can now trust what is inside the control block. */

    /* The version tag is for future extensions after the format
       stabilizes and should be 0 for now. */
    if (control->version != 0) return 0;

    /* Compute and check firmware CRC. */
    uint32_t computed_fw_crc = crc(start, endx-start);
    if (log) { log->crc_fw = computed_fw_crc; }
    if (control->fw_crc != computed_fw_crc) return 0;

    /* We're good.  Caller can trust contents of control block to make
       boot decisions other than version compare. */
    return control;
}


#endif
