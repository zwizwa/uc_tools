#ifndef MOD_MEM_WRITE_STM32F103
#define MOD_MEM_WRITE_STM32F103

/* Perform a Flash write, with added memory protection.
   Return value:

    0 OK
   -1 ERROR operation in progress
      from hw_flash_check_eop() in hw_flash_write_and_erase()
   -2 to -6 ERROR: memory protection errors: see hw_mem_write() for details.

   [tom] I've seen -1 happen when bad addresses are written on
   STM32F103.  I have no explanation of why the Flash programming
   seems to get stuck after that.  A reboot is needed to resolve.

*/

/* Logging is off by default to not interfere with core dumps. */
#ifndef MEM_WRITE_LOG
#define MEM_WRITE_LOG(...)
#endif


#include "memory.h"

int32_t hw_mem_write_(uint32_t addr, const uint8_t *buf, uint32_t len) {
#if 1
    return hw_flash_write_and_erase(10 /*page_logsize*/, addr, buf, len);
#else
    /* THIS IS ONLY FOR TESTING AND DOES NOT WORK FOR INCREMENTAL
     * FIRMWARE UPDATE WRITES. */
    int32_t rv;
    uint32_t t0 = cycle_counter();
    if ((rv = hw_flash_erase(addr, (1<<10), 10))) return rv;
    uint32_t t1 = cycle_counter();
    if ((rv = hw_flash_write(addr, buf, len))) return rv;
    uint32_t t2 = cycle_counter();
    MEM_WRITE_LOG("hw_mem_write_ us1=%d, us2=%d\n",
                  (t1-t0)/HW_CPU_MHZ,
                  (t2-t1)/HW_CPU_MHZ);
    return rv;
#endif
}

#ifndef MEM_WRITE_FLASH_START
extern struct gdbstub_config config;
#define MEM_WRITE_FLASH_START ((uint32_t)config.flash_start)
#define MEM_WRITE_FLASH_ENDX  ((uint32_t)config.flash_endx)
#endif

int32_t hw_mem_write(uint32_t addr, const uint8_t *buf, uint32_t len) {

    const uint32_t block_size = 1024; // FIXME: hardcoded
    /* Memory protection.  Only allow access to partitions, to code
       that is not runing. */
    /* FIXME: propery define status codes? */
    if (addr     < 0x08004000) return -2; // before start of partitions
    if (addr+len > 0x08020000) return -3; // after end of partitions

    /* This is no longer block-aligned, so do block alignment here. */
    uint32_t endx = ((((MEM_WRITE_FLASH_ENDX)-1)/block_size)+1)*block_size;

    if ((addr     >= MEM_WRITE_FLASH_START) &&
        (addr+len <= (endx + block_size))) {
        return -4; // inside running partition (fw image + fw control block)
    }

    if (addr&1) return -5; // bad alignment
    if (len&1)  return -6; // bad alignment

    MEM_WRITE_LOG("set_c8_memory %x:%d FLASH\n", addr, len);
    //info_hex_u8(buf, len); MEM_WRITE_LOG("\n");
    return hw_mem_write_(addr, buf, len);
}

int32_t hw_mem_write_log(uint32_t addr, const uint8_t *buf, uint32_t len) {
    uint32_t stamp = cycle_counter();
    int32_t  rv    = hw_mem_write(addr, buf, len);
    uint32_t diff  = cycle_counter() - stamp;
    (void)diff;
    MEM_WRITE_LOG("hw_mem_write rv=%d, ms=%d\n", rv, diff / (HW_CPU_MHZ * 1000));
    return rv;
}

#endif
