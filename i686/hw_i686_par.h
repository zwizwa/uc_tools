#ifndef HW_I686_PAR_H
#define HW_I686_PAR_H

/* SPP (Standard Parallel Port) registers - BAR0, 8 bytes */
#define SPP_DATA        0x00    /* Data register (R/W in bidirectional mode) */
#define SPP_STATUS      0x01    /* Status register (read-only) */
#define SPP_CONTROL     0x02    /* Control register (R/W) */

/* EPP (Enhanced Parallel Port) registers - BAR0, offsets 3-7 */
#define EPP_ADDR        0x03    /* EPP address register */
#define EPP_DATA        0x04    /* EPP data register (8/16/32-bit) */
                                /* 0x05-0x07: EPP data bytes 2-4 for 32-bit transfers */

/* ECP (Extended Capabilities Port) registers - BAR1, 8 bytes */
#define ECP_DATA_FIFO   0x00    /* ECP data FIFO (also cnfgA in config mode) */
#define ECP_CNFG_B      0x01    /* Configuration B (in config mode) */
#define ECP_ECR         0x02    /* Extended Control Register */

/* SPP_STATUS bits (note: BUSY and ERROR are inverted in hardware) */
#define SPP_STATUS_BUSY    0x80    /* 0 = busy, 1 = ready (inverted) */
#define SPP_STATUS_ACK     0x40    /* Acknowledge */
#define SPP_STATUS_PE      0x20    /* Paper empty */
#define SPP_STATUS_SELECT  0x10    /* Select */
#define SPP_STATUS_ERROR   0x08    /* 0 = error (inverted) */

/* SPP_CONTROL bits */
#define SPP_CONTROL_STROBE   0x01  /* Strobe (inverted on pin) */
#define SPP_CONTROL_AUTOFD   0x02  /* Auto linefeed (inverted on pin) */
#define SPP_CONTROL_INIT     0x04  /* Initialize (NOT inverted) */
#define SPP_CONTROL_SELECT   0x08  /* Select printer (inverted on pin) */
#define SPP_CONTROL_IRQ_EN   0x10  /* Enable IRQ via ACK */
#define SPP_CONTROL_BIDIR    0x20  /* 1 = input mode, 0 = output mode */

/* ECR modes (upper 3 bits of ECP_ECR) */
#define ECR_MODE_SPP        0x00   /* Standard mode */
#define ECR_MODE_PS2        0x20   /* Bidirectional (byte) mode */
#define ECR_MODE_FIFO       0x40   /* Parallel port FIFO */
#define ECR_MODE_ECP        0x60   /* ECP FIFO */
#define ECR_MODE_EPP        0x80   /* EPP mode */
#define ECR_MODE_TEST       0xC0   /* Test mode */
#define ECR_MODE_CFG        0xE0   /* Configuration mode */

#include "hw_i686_io.h"


struct par {
    uint16_t spp;
    uint16_t epp;
};

static inline void par_write_data(struct par *p, uint8_t data) {
    outb(p->spp + SPP_DATA, data);
}
static inline void par_pulse(struct par *p) {
    LOG("par_pulse\n");
    par_write_data(p, 0xFF);
    par_write_data(p, 0x00);
}

#endif


