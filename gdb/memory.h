#ifndef MEMORY_H
#define MEMORY_H

#include <stdint.h>
#include <libopencm3/stm32/flash.h>

// This is platform-dependent.  Only F1 is supported for now

#if defined(STM32F1)

/* Don't use the library routines.  Instead move functionality into
   inline functions to make it easier to include in the bl_ram.h
   trampoline.

   Code for memory size >512kByte is removed.  See libopencm3 routines.
*/
#if 1
static inline uint32_t hw_flash_get_status_flags(void) {
    uint32_t flags = (FLASH_SR & (FLASH_SR_PGERR |
                                  FLASH_SR_EOP |
                                  FLASH_SR_WRPRTERR |
                                  FLASH_SR_BSY));
    return flags;
}
static inline void hw_flash_wait_for_last_operation(void) {
    while ((hw_flash_get_status_flags() & FLASH_SR_BSY) == FLASH_SR_BSY);
}
static inline void hw_flash_erase_page(uint32_t page_address) {
    hw_flash_wait_for_last_operation();
    FLASH_CR |= FLASH_CR_PER;
    FLASH_AR = page_address;
    FLASH_CR |= FLASH_CR_STRT;
    hw_flash_wait_for_last_operation();
    FLASH_CR &= ~FLASH_CR_PER;
}
 inline void hw_flash_lock(void) {
    //__asm__ volatile("cpsid if\n");
    FLASH_CR |= FLASH_CR_LOCK;
}
static inline void hw_flash_unlock(void) {
    FLASH_CR |= FLASH_CR_LOCK;
    FLASH_KEYR = FLASH_KEYR_KEY1;
    FLASH_KEYR = FLASH_KEYR_KEY2;
    //__asm__ volatile("cpsie if\n");
}
static inline void hw_flash_program_half_word(uint32_t address, uint16_t data) {
    hw_flash_wait_for_last_operation();
    FLASH_CR |= FLASH_CR_PG;
    MMIO16(address) = data;
    hw_flash_wait_for_last_operation();
    FLASH_CR &= ~FLASH_CR_PG;
}
#else // if 1
// For testing only: delegate to to libopencm3 calls.
static inline void hw_flash_erase_page(uint32_t addr) {
    return flash_erase_page(addr);
}
static inline uint32_t hw_flash_get_status_flags(void) {
    return flash_get_status_flags();
}
static inline void hw_flash_lock(void) {
    flash_lock();
}
static inline void hw_flash_unlock(void) {
    flash_unlock();
}
static inline void hw_flash_program_half_word(uint32_t address, uint16_t data) {
    flash_program_half_word(address, data);
}

#endif // if 1



static inline int32_t hw_flash_check_eop(void) {
    // Check status.  Is this really necessary?
    uint32_t flash_status = hw_flash_get_status_flags();
    return (flash_status != FLASH_SR_EOP) ? -1 : 0;
}
static inline int32_t hw_flash_erase(uint32_t addr, uint32_t size,
                                     uint32_t flash_page_size_log) {
    int32_t rv = 0;
    const uint32_t page_size = 1 << flash_page_size_log;
    uint32_t num_pages = ((size-1) >> flash_page_size_log)+1; // round up
    hw_flash_unlock();
    while (num_pages--) {
        hw_flash_erase_page(addr);
        if ((rv = hw_flash_check_eop())) break;
        addr += page_size;
    }
    hw_flash_lock();
    return rv;
}
static inline int32_t hw_flash_write(uint32_t addr, const uint8_t *b_buf,
                                     uint32_t len) {
    int32_t rv = 0;
    uint16_t *hw_buf = (uint16_t *)b_buf; // FIXME: unaligned access allowed?
    uint32_t nb_half_words = len / 2;
    hw_flash_unlock();
    while (nb_half_words--) {
        hw_flash_program_half_word(addr, *hw_buf);
        if ((rv = hw_flash_check_eop())) break;
        addr += 2;
        hw_buf += 1;
    }
    hw_flash_lock();
    return rv;
}
/* Write + erase automatically on page boundary.
   Return value:
   -1 ERROR operation in progress  (from hw_flash_check_eop())
    0 OK
*/
static inline int32_t hw_flash_write_and_erase(
    uint32_t flash_page_size_log,
    uint32_t addr, const uint8_t *b_buf,
    uint32_t len) {

    uint32_t page_mask = (1 << flash_page_size_log) - 1;
    int32_t rv = 0;
    uint16_t *hw_buf = (uint16_t *)b_buf; // FIXME: unaligned access allowed?
    uint32_t nb_half_words = len / 2;
    hw_flash_unlock();
    while (nb_half_words--) {

        if (!(addr & page_mask)) {
            hw_flash_erase_page(addr);
            if ((rv = hw_flash_check_eop())) break;
        }

        hw_flash_program_half_word(addr, *hw_buf);
        if ((rv = hw_flash_check_eop())) break;
        addr += 2;
        hw_buf += 1;
    }
    hw_flash_lock();
    return rv;
}

/* Macros for protothread variant.
   This is just a dumb translation. */

#define HW_FLASH_WAIT_FOR_LAST_OPERATION(sm) \
    SM_WHILE(sm, (hw_flash_get_status_flags() & FLASH_SR_BSY) == FLASH_SR_BSY)



#else // defined (STM32F1)

//#warning NO_FLASH_SUPPORT

static inline int32_t hw_flash_erase(uint32_t addr, uint32_t size,
                                     uint32_t flash_page_size_log) {
    return 0;
}
static inline int32_t hw_flash_write(uint32_t addr, const uint8_t *b_buf,
                                     uint32_t len) {
    return 0;
}
static inline int32_t hw_flash_write_and_erase(
    uint32_t flash_page_size_log,
    uint32_t addr, const uint8_t *b_buf,
    uint32_t len) {

    return 0;

#endif // defined (STM32F1)

#endif // MEMORY_H

