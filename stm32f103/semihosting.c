#include "semihosting.h"

/*

https://www.keil.com/support/man/docs/armcc/armcc_pge1358787048379.htm

If you are compiling for the ARMv6-M or ARMv7-M, the Thumb BKPT
instruction is used instead of the Thumb SVC instruction. Both BKPT
and SVC take an 8-bit immediate value. In all other respects,
semihosting is the same for all supported ARM processors.

https://en.wikipedia.org/wiki/ARM_Cortex-M#Cortex-M0
https://en.wikipedia.org/wiki/ARM_Cortex-M#Cortex-M3

M0: ARMv6-M
M3: ARMv7-M

The ASM is adapted from:
http://embed.rs/articles/2016/semi-hosting-rust/

*/

uint32_t semihosting_call(uint32_t num, void *addr) {
    uint32_t result;
    __asm__ volatile (
        "mov r0, %1\n\t"
        "mov r1, %2\n\t"
        "bkpt 0xAB\n\t"
        "mov %0, r0"
        : "=r"(result)
        : "r"(num),"r"(addr)
        : "r0", "r1");
    return result;
}
struct semihosting_write {
    uint32_t fd;
    const uint8_t *addr;
    uint32_t len;
};
void semihosting_write(uint32_t fd, const uint8_t *addr, uint32_t len) {
    struct semihosting_write arg = { fd, addr, len };
    semihosting_call(0x05, &arg);
}

/* Send info log to stdout, to keep it separate from stderr which is
   used for openocd log messages. */
uint32_t info_bytes(void);
uint32_t info_read(uint8_t *buf, uint32_t len);
void semihosting_info_poll(void) {
    uint32_t len = info_bytes();
    if (len) {
        uint8_t buf[len];
        info_read(buf, sizeof(buf));
        semihosting_write(1, buf, sizeof(buf));
    }
}

