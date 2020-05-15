#ifndef DATA_H
#define DATA_H

#include "gdbstub_api.h"


/* Last segment of uC program Flash is used as data store. It can be
   updated independently of firmware image. See data.ld */

struct binary {
    uint32_t type; // default=0: raw binary, app knows how to interpret
    uint32_t size;
    const uint8_t *data;
    const struct binary *next;
};

struct store {
    const struct binary *first;
};
extern const struct store _store;
extern const uint32_t _flash_top;
static inline int have_store(void) {
    return ((void*)&_flash_top) > ((void*)&_store);
}

#endif
