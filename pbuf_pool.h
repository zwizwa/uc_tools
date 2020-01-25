#ifndef PBUF_POOL_H
#define PBUF_POOL_H

#include "pbuf.h"
#include "cbuf.h"

/* A simple buffer pool, implemented as a cbuf containting references
 * to a pbuf array. */
struct pbuf_pool {
    struct cbuf *free;
    struct pbuf *pool;
    uint32_t nb_bufs;
};

/* FIXME: Note that a stack could be used as well, e.g. linking
 * together pbuf_pool structs, but that would not make it safe to use
 * in pre-emptive scenarios. */


#endif
