#include "memory.h"
#include "gdbstub.h"

extern const uint32_t flash_page_size_log;

/* Adapts libopencm3 Flash driver to gdbstub.c */
int32_t flash_erase(uint32_t addr, uint32_t size) {
    return hw_flash_erase(addr, size, flash_page_size_log);
}
int32_t flash_write(uint32_t addr, const uint8_t *b_buf, uint32_t len) {
    return hw_flash_write(addr, b_buf, len);
}

/* Just let GDB access everything.  Trust the memory map. */
uint8_t mem_read(uint32_t addr) {
    return *(uint8_t*)(addr);
}
int32_t mem_write(uint32_t addr, uint8_t val) {
    *(uint8_t*)(addr) = val; return E_OK;
}
int32_t mem_write32(uint32_t addr, uint32_t val) {
    *(uint32_t*)(addr) = val; return E_OK;
}

#if 0
/* STM32F103 memory is organized in blocks of
   0x20000000 512k (1<<29) bytes 
   Only allow access to regions that don't cause exceptions.
   FIXME: Catch exception instead! */

uint8_t mem_read(uint32_t addr) 
    switch(addr >> 29) {
    case 0x00000000 >> 29: // Flash
    case 0x20000000 >> 29: // RAM
    case 0x40000000 >> 29: // Peripherals
        return *(uint8_t*)(addr);
    default:
        return 0xFF;
    }
}
int32_t mem_write(uint32_t addr, uint8_t val) {
    switch(addr >> 29) {
    case 0x20000000 >> 29: // RAM
    case 0x40000000 >> 29: // Peripherals
        *(uint8_t*)(addr) = val;
        return E_OK;
    default:
        return E_MEM;
    }
}
#endif
