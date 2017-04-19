#ifndef BOARD_H
#define BOARD_H
#include "generic.h"


//usbd_device *bootloader_init(void);
//void bootloader_poll(usbd_device *usbd_dev);
void bootloader_init(void);
void bootloader_poll(void);

uint32_t bootloader_read(uint8_t *buf, uint32_t size);
void     bootloader_write(const uint8_t *buf, uint32_t size);

static inline void bootloader_loop(void) {
    for(;;) {
        bootloader_poll();
    }
}


#endif // BOARD_H
