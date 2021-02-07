#ifndef SEMIHOSTING_H
#define SEMIHOSTING_H

#include <stdint.h>
void semihosting_write(uint32_t fd, const uint8_t *addr, uint32_t len);
void semihosting_info_poll(void);

#endif
