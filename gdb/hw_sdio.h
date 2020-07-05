#ifndef HW_SDIO_H
#define HW_SDIO_H

#include "base.h"
#include <libopencm3/stm32/dma.h>
#include <libopencm3/stm32/sdio.h>
#include <libopencm3/stm32/rcc.h>
#include <stdint.h>
#include <string.h>
#include "storage_dimensions.h"
#include "sdio_cmd.h"

/* SDIO */

// Only works with 512 byte blocks
CT_ASSERT(sd_size, SD_UNIT_SIZE == 512);

/* This is fixed for SDHC cards, so just fix it for everything.
   Use CMD25 WRITE_MULTIPLE_BLOCK to send more data at once. */
#define SD_UNIT_WORD_SIZE (1 << (SD_UNIT_LOGSIZE - 2))

// removed old ISR code after git f63967dd698e56e2887f1533bb5f639175e09786



/* DMA example in ref man for single block write:
   page 571, section 22.3.2 SDIO AHB interface */

union hw_sdio_sta {
    uint32_t word;
    struct {
        unsigned int CCRCFAIL :1; // 0
        unsigned int DCRCFAIL :1; // 1
        unsigned int CTIMEOUT :1; // 2
        unsigned int DTIMEOUT :1; // 3
        unsigned int TXUNDERR :1; // 4
        unsigned int RXOVERR  :1; // 5
        unsigned int CMDREND  :1; // 6
        unsigned int CMDSENT  :1; // 7
        unsigned int DATAEND  :1; // 8
        unsigned int STBITERR :1; // 9
        unsigned int DBCKEND  :1; // 10
        unsigned int CMDACT   :1; // 11
        unsigned int TXACT    :1; // 12
        unsigned int RXACT    :1; // 13
        unsigned int TXFIFOHE :1; // 14
        unsigned int RXFIFOHF :1; // 15
        unsigned int TXFIFOF  :1; // 16
        unsigned int RXFIFOF  :1; // 17
        unsigned int TXFIFOE  :1; // 18
        unsigned int RXFIFOE  :1; // 19
        unsigned int TXDAVL   :1; // 20
        unsigned int RXDAVL   :1; // 21

    } bits;
};


/* SDIO ISR log */

// Use unused top 8 bits from SDIO_STA to signal some highlevel errors.
#define HW_SDIO_ERROR_BADCARD (1 << 26)
#define HW_SDIO_ERROR_ALIGN   (1 << 25)
#define HW_SDIO_ERROR_OPCOND  (1 << 24)

// Errors only
#define HW_SDIO_STATUS_ERROR_MASK               \
    (SDIO_MASK_CCRCFAILIE |                     \
     SDIO_MASK_DCRCFAILIE |                     \
     SDIO_MASK_CTIMEOUTIE |                     \
     SDIO_MASK_DTIMEOUTIE |                     \
     SDIO_MASK_TXUNDERRIE |                     \
     SDIO_MASK_RXOVERRIE  |                     \
     SDIO_MASK_STBITERRIE)

// OK + errors
#define HW_SDIO_MASK                            \
    (SDIO_MASK_CMDRENDIE |                      \
     SDIO_MASK_CMDSENTIE |                      \
     SDIO_MASK_DBCKENDIE |                      \
     HW_SDIO_STATUS_ERROR_MASK)

struct hw_sdio_stats {
    volatile uint32_t start;   // start count
    volatile uint32_t isr;     // isr count
    volatile uint32_t cmdrend; // successful command count
    volatile uint32_t dbckend; // successful data count (crc check passed)
    volatile uint32_t retry;   // command timeout retries
    volatile union {
        // The lower 10 bits 0x3FF contain error flags.  Each of these
        // has a separate counter.  Named struct is for gdb display.
        uint32_t indexed[10];
        struct {
            uint32_t CCRCFAIL ;
            uint32_t DCRCFAIL ;
            uint32_t CTIMEOUT ;
            uint32_t DTIMEOUT ;
            uint32_t TXUNDERR ;
            uint32_t RXOVERR  ;
            uint32_t CMDREND  ;
            uint32_t CMDSENT  ;
            uint32_t DATAEND  ;
            uint32_t STBITERR ;
        } named;
    } count;
};

struct hw_sdio_card {
    uint32_t    props;
    uint32_t    ocr;
    uint32_t    cid[4];
    uint32_t    csd[4];
    uint32_t    scr[2];
    uint32_t    ssr[16];
    uint32_t    nb_units; // nb of SD_UNIT_SIZE byte blocks
    uint16_t    rca;
    struct hw_sdio_stats stats;
};

struct hw_sdio {
    uint32_t write; // 1=write, 0=read
    struct hw_dma d;
};


// FIXME: remove usage of HW_STRUCT
#define HW_STRUCT(name,init) ({ struct name c = init; c; })
#define HW_SDIO_WRITE { 1, { RCC_DMA2, 0/*irq not used*/, DMA2, DMA_CHANNEL4 } }
#define HW_SDIO HW_STRUCT(hw_sdio, HW_SDIO_WRITE)

INLINE uint32_t hw_sdio_clkdiv(uint32_t freq_hz) {
    const uint32_t sdioclk_hz = 72000000; // same as HCLK
    uint32_t ratio = sdioclk_hz / freq_hz;
    return ratio - 2;
}

#define HW_SDIO_1BIT 0
#define HW_SDIO_4BIT 1


INLINE void hw_sdio_set_bus(int wide, uint32_t freq_hz) {
    SDIO_CLKCR =
        SDIO_CLKCR_CLKEN |
        SDIO_CLKCR_HWFC_EN | // hardware flow control (see refman 22.8)
        (wide ? SDIO_CLKCR_WIDBUS_4 : SDIO_CLKCR_WIDBUS_1) |
        (0xFF & hw_sdio_clkdiv(freq_hz));
}


/* Write Single Block, DMA

   Sequencing is done in two parts:

   1) Start command transfer in time slot after DTI SPI receive is set up.

     - check previous DBCKEND and error condition -> retry, or transfer next
     - setup DMA for data transfer
     - start command transmission

   2) CMDREND / error interrupt:

     - retry or abort if errors
     - start data transfer from DMA

   Catching DMA done interrupt isn't necessary.


   This follows refman 22.3.2

   4b) DMA2:4: source = mem, dest = SDIO_FIFO
   4c) DMA2:4 mem inc, no per inc, word size
   4d) Enable DMA2:4
   5a) SDIO data length reg (timeout programmed before identification) !!
   5b) SDIO address = block number
   5c) SDIO command = 24, WRITE_BLOCK,  waitresp=1, CPSMEN=1
   5d) Wait for SDIO_STA6 = CMDREND interrupt, then set DTEN=1 (send data), DTDIR=0, DTMODE=0, DMAENM=1, DBLOCKSIZE=9
   5e) Wait for SDIO_STA = DBCKEND


   Note: Code only uses WRITE_MULTIPLE_BLOCK for performance reasons.

*/


INLINE void hw_sdio_write_unit_dma_init(void *data, uint32_t nb_32bit_words) {

    struct hw_sdio c = HW_SDIO;

    /* Disable channel and reset config bits. */
    DMA_CCR(c.d.dma, c.d.chan) = 0;

    /* Reset interrupt flags. */
    DMA_IFCR(c.d.dma) |= DMA_IFCR_CIF(c.d.chan);

    /* 1-2-3 peripheral address, memory address, transfer size */
    DMA_CPAR(c.d.dma, c.d.chan) = (uint32_t)(&SDIO_FIFO);
    DMA_CMAR(c.d.dma, c.d.chan) = (uint32_t) data;
    DMA_CNDTR(c.d.dma, c.d.chan) = nb_32bit_words;

    /* 4 Priority */
    DMA_CCR(c.d.dma, c.d.chan) = DMA_CCR_PL_LOW;  // low, because DTI is high pri

    /* 5 DMA configuration register */
    DMA_CCR(c.d.dma, c.d.chan) |=
          DMA_CCR_MINC                  // memory increment mode
        | DMA_CCR_PSIZE_32BIT           // peripheral access
        | DMA_CCR_MSIZE_32BIT           // memory access
        | (c.write ? DMA_CCR_DIR : 0);  // direction
    //  | DMA_CCR_TCIE                  // don't enable transfer complete interrupt

    /* 6 Enable */
    DMA_CCR(c.d.dma, c.d.chan) |= DMA_CCR_EN;

    /* DMA transfer will start when DCTRL:DTEN is set to 1 after
     * completing the command transmission, signalled by CMDREND. */
}



INLINE void hw_sdio_sta_record_errors(struct hw_sdio_card *sd, uint32_t sta) {
    uint32_t error_flags = sta & HW_SDIO_STATUS_ERROR_MASK;
    if (sta & error_flags) {
        SDIO_ICR = error_flags; // ack
        for (int i = 0; i < 10; i++) {
            if (error_flags & (1 << i)) { sd->stats.count.indexed[i]++; }
        }
    }
}



/* POLLED */
INLINE uint32_t hw_sdio_cmd_response_length(uint32_t cmd) {
    switch(cmd) {
    case 0:  return SDIO_CMD_WAITRESP_NO_0;
    case 2:
    case 9:  return SDIO_CMD_WAITRESP_LONG;
    default: return SDIO_CMD_WAITRESP_SHORT; // the common case
    }
}
INLINE void hw_sdio_command_send(uint32_t cmd, uint32_t arg) {
    SDIO_ICR = 0x7ff;
    SDIO_ARG = arg;
    SDIO_CMD =
        (SDIO_CMD & ~0x7ff) |              // preserve bits
        (cmd & SDIO_CMD_CMDINDEX_MASK) |   // what to send
        hw_sdio_cmd_response_length(cmd) | // what to wait for
        SDIO_CMD_CPSMEN;                   // command state machine enable
}

/* Return 0 if sta_ok is reached, otherwise return error flags and update log. */
INLINE uint32_t hw_sdio_sta_wait(void) {
    uint32_t sta;
    while (!((sta = SDIO_STA) & HW_SDIO_MASK)); // same as interrupt routine
    SDIO_ICR = sta; // ack everything
    return sta;
}

INLINE uint32_t hw_sdio_sta_check(struct hw_sdio_card *sd,
                                  uint32_t sta, uint32_t sta_ok) {

    if (sta & sta_ok) {
        return 0;
    }
    else {
        hw_sdio_sta_record_errors(sd, sta);
        return sta;
    }
}
uint32_t hw_sdio_command(struct hw_sdio_card *sd, uint32_t cmd, uint32_t arg);

// FIXME: difference with hw_sdio_command?
void hw_sdio_cmd(uint32_t cmd, uint32_t arg);
void hw_sdio_send_unit(void *data, uint32_t logsize);


/* Only one, so no need to keep referring to it.. */
extern struct hw_sdio_card sd;
extern struct hw_sdio_stats stats;

void hw_sdio_init(void);
void hw_sdio_reset(int on);
uint32_t hw_sdio_open(struct hw_sdio_card *sd);
uint32_t hw_sdio_read_unit(struct hw_sdio_card *sd, uint32_t lba, uint8_t *buf);
uint32_t hw_sdio_write_unit(struct hw_sdio_card *sd, uint32_t lba, uint8_t *buf);
uint32_t hw_sdio_erase(struct hw_sdio_card *sd, uint32_t start, uint32_t end);
uint32_t hw_sdio_select(struct hw_sdio_card *sd, int rca);

INLINE int hw_sdio_card_ccs(void) {
    // Card capacity status. Physical layer simplified spec: 5.1 ocr
    return (sd.ocr & 0x40000000) != 0;
}

INLINE uint32_t hw_sdio_card_lba_to_addr(uint32_t lba) {
    return hw_sdio_card_ccs() ? lba : lba * SD_UNIT_SIZE;
}



/*
 * Helper functions to pull out various bit fields of the CSD for the
 * size calculation.
 */

INLINE uint32_t hw_sdio_csd(uint32_t end, uint32_t start) { return bitslice(sd.csd, end, start); }
INLINE uint32_t hw_sdio_ssr(uint32_t end, uint32_t start) { return bitslice(sd.ssr, end, start); }

INLINE int hw_sdio_csd_structure()   { return hw_sdio_csd(127, 126); }
INLINE int hw_sdio_read_bl_len()     { return hw_sdio_csd( 83,  80); }
INLINE int hw_sdio_c_size_mult_v1()  { return hw_sdio_csd( 49,  47); }
INLINE int hw_sdio_c_size_v1()       { return hw_sdio_csd( 73,  62); }
INLINE int hw_sdio_c_size_v2()       { return hw_sdio_csd( 69,  48); }

INLINE int hw_sdio_au_size()         { return hw_sdio_ssr( 431, 428); }
INLINE int hw_sdio_erase_size()      { return hw_sdio_ssr( 423, 408); }

#include "sm_sdio.h"


int hw_sdio_ready(void);


#endif // HW_SDIO_H
