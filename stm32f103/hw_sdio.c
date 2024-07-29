#include "base.h"
#include "sm.h"

#include <stdint.h>
#include <string.h>
#include <libopencm3/stm32/sdio.h>
#include <libopencm3/stm32/gpio.h>
#include <libopencm3/stm32/rcc.h>
#include <libopencm3/cm3/nvic.h>
#include <stdio.h>

#include "hw_sdio.h"

#include "sdio_cmd.h"

#define DEBUG 0

/* Compute this from the buffer depth.  The idea is to be able to
   allow delay spikes that can be compensated by buffering, but raise
   an error on longer delays to be able to restart the state machine. */
uint32_t dtimer_init = 0x1000000;





/* DMA MODE.
   Use polling for status flags but use DMA for data unit transfers.

   It should be possible to run this from interrupts using SDIO
   interrupts, and EXTI for polling the D0 busy condition.  However
   EXTI is edge triggered, so be ware about the race condition between
   EXTI interrupt enable and the arrival of the edge.  It would have
   been nice of the SDIO peripheral handled busy conditions, or if
   there were level triggered EXTI interrupts.
*/

#ifndef LOG_SDIO
#define LOG_SDIO(...)
//#define LOG_SDIO LOG
#endif



void hw_sdio_cmd(uint32_t cmd, uint32_t arg) {
    LOG_SDIO("hw_sdio_cmd(%d,%d)\n", (int)cmd, (int)arg);
    SDIO_ICR = -1; // ack any pending
    SDIO_ARG = arg;
    SDIO_CMD =
        (SDIO_CMD & ~(0x7FF)) | // save pre-existing state
        (cmd & SDIO_CMD_CMDINDEX_MASK) |
        SDIO_CMD_CPSMEN |
        SDIO_CMD_WAITRESP_SHORT;
}


void hw_sdio_send_unit(void *data, uint32_t logsize) {
    LOG_SDIO("hw_sdio_send_unit(%08x,%d)\n",(unsigned int)data,(int)logsize);
    uint32_t nb_32bit_words = 1 << (logsize - 2);
    hw_sdio_write_unit_dma_init(data, nb_32bit_words);

    /* Initiate new data transfer */
    // 0x1000, 0x8000 gives timeout on first write multiple
    // How to pick a good value?  Leave enough time to retry once.
    SDIO_DTIMER = dtimer_init;
    SDIO_DLEN = 1 << logsize;
    SDIO_DCTRL =
        (logsize << SDIO_DCTRL_DBLOCKSIZE_SHIFT) |
        SDIO_DCTRL_DMAEN | // use dma
        SDIO_DCTRL_DTEN;   // send data.
}


int hw_sdio_ready(void) { return hw_gpio_read(GPIOC, 8); }








/* POLLED MODE */
uint32_t hw_sdio_command(struct hw_sdio_card *sd,
                         uint32_t cmd, uint32_t arg) {
    SDIO_MASK = 0; // interrupts off
    hw_sdio_command_send(cmd, arg);
    uint32_t sta = hw_sdio_sta_wait();
    if (DEBUG) { printf("CMD %2d (0x%08x) -> 0x%08x\n",
                        (int)cmd, (unsigned int)arg, (unsigned int)sta); }
    return hw_sdio_sta_check(sd, sta, SDIO_STA_CMDREND | SDIO_STA_CMDSENT);
}
uint32_t hw_sdio_select(struct hw_sdio_card *sd, int rca) {
    uint32_t sta = hw_sdio_command(sd, SDIO_CMD_SELECT, rca << 16);
    if ((rca == 0) && (sta & SDIO_STA_CTIMEOUT)) return 0; // select 0 -> timeout == deselect
    return sta;
}

uint32_t hw_sdio_command_read_data(struct hw_sdio_card *sd,
                                   uint32_t cmd, uint32_t addr,
                                   uint32_t *buf32, uint32_t buf_logsize) {
    SDIO_DTIMER = 0xffffffff; // FIXME: this is too much
    SDIO_DLEN = 1 << buf_logsize;
    SDIO_DCTRL =
        (buf_logsize << SDIO_DCTRL_DBLOCKSIZE_SHIFT) |
        SDIO_DCTRL_DTDIR |
        SDIO_DCTRL_DTEN;

    TRY(hw_sdio_command(sd, cmd, addr));

    uint32_t word_endx = 1 << (buf_logsize - 2);
    uint32_t nb_words = 0;

    if (word_endx >= 8) {
        // Chunked read
        uint32_t chunk_endx = word_endx / 8;
        word_endx = 8;
        for (uint32_t chunk = 0; chunk < chunk_endx; chunk++) {
            while(!(SDIO_STA & SDIO_STA_RXFIFOHF)); // FIXME: can this freeze?
            for (int word = 0; word < word_endx; word++) {
                buf32[nb_words++] = SDIO_FIFO;
            }
        }
    }

    else {
        // Single word read
        for (uint32_t word = 0; word < word_endx; word++) {
            while(!(SDIO_STA & SDIO_STA_RXDAVL));
            buf32[nb_words++] = SDIO_FIFO;
        }
    }
    while (SDIO_STA & SDIO_STA_RXACT); // wait until done
    return hw_sdio_sta_check(sd, SDIO_STA, SDIO_STA_DBCKEND);
}

uint32_t hw_sdio_read_unit(struct hw_sdio_card *sd, uint32_t lba, uint8_t *buf) {
    uint32_t addr = hw_sdio_card_lba_to_addr(sd, lba);

    if ((((uint32_t)buf) & 3) != 0) return HW_SDIO_ERROR_ALIGN;
    uint32_t *buf32 = (void*)buf; // buffer should be aligned

    TRY(hw_sdio_command(sd, SDIO_CMD_SET_BLOCKLEN, SD_UNIT_SIZE));
    return hw_sdio_command_read_data(sd, SDIO_CMD_READ_SINGLE_BLOCK, addr, buf32, SD_UNIT_LOGSIZE);
}



/*
 * Set up the GPIO pins and peripheral clocks for the SDIO
 * system. The code should probably take an option card detect
 * pin, at the moment it uses the one used by the Embest board.
 */
void hw_sdio_init(void) {

    struct hw_sdio c = HW_SDIO;
    SDIO_MASK = 0;

    /* Enable clocks for SDIO and DMA */
    rcc_periph_clock_enable(RCC_SDIO);
    rcc_periph_clock_enable(RCC_GPIOC);
    rcc_periph_clock_enable(RCC_GPIOD);
    rcc_periph_clock_enable(c.d.rcc_dma);

    /* SD bus pins */
    hw_gpio_config(GPIOC,  8, HW_GPIO_CONFIG_ALTFN); // D0
    hw_gpio_config(GPIOC,  9, HW_GPIO_CONFIG_ALTFN); // D1
    hw_gpio_config(GPIOC, 10, HW_GPIO_CONFIG_ALTFN); // D2
    hw_gpio_config(GPIOC, 11, HW_GPIO_CONFIG_ALTFN); // D3
    hw_gpio_config(GPIOC, 12, HW_GPIO_CONFIG_ALTFN); // CLK
    hw_gpio_config(GPIOD,  2, HW_GPIO_CONFIG_ALTFN); // CMD
    hw_gpio_config(GPIOD,  3, HW_GPIO_CONFIG_INPUT); // Card Select on powermcu board
}

void hw_sdio_reset(int on) {

    /* Step 1 power off the interface */
    SDIO_POWER = SDIO_POWER_PWRCTRL_PWROFF;
    /* reset the SDIO peripheral interface */
    //delay is required to allow it to activate, at least 7 clock cycles
    hw_busywait(10*24);
    if (on) {
        SDIO_POWER = SDIO_POWER_PWRCTRL_PWRON;
        hw_sdio_set_bus(HW_SDIO_1BIT, 400000);
    }
}

// Use the write multiple state machine for high performance.
uint32_t hw_sdio_write_unit(struct hw_sdio_card *sd, uint32_t lba, uint8_t *buf) {
    uint32_t addr = hw_sdio_card_lba_to_addr(sd, lba);

    if ((((uint32_t)buf) & 3) != 0) return HW_SDIO_ERROR_ALIGN;
    uint32_t *buf32 = (void*)buf;

    //reset the SDIO timeout register, set the SDIO data length and unit size
    SDIO_DTIMER = 0xffffffff;
    SDIO_DLEN = SD_UNIT_SIZE;
    SDIO_DCTRL = SDIO_DCTRL_DBLOCKSIZE_9 | SDIO_DCTRL_DTEN;

    TRY(hw_sdio_command(sd, SDIO_CMD_WRITE_SINGLE_BLOCK, addr)); // write single unit

    uint32_t nb_words = 0;
    do {
        if (SDIO_STA & SDIO_STA_TXFIFOHE) { // 8 words available
            for (int i = 0; i<8; i++) {
                SDIO_FIFO = buf32[nb_words++];
            }
        }
    } while ((SDIO_STA & SDIO_STA_TXACT) && (nb_words < 128));
    while (SDIO_STA & SDIO_STA_TXACT);

    return hw_sdio_sta_check(sd, SDIO_STA, SDIO_STA_DBCKEND);
}



INLINE void swap_buf(uint32_t *buf, uint32_t n) {
    for (uint32_t i=0; i < n/2; i++) {
        uint32_t a = buf[i];
        uint32_t b = buf[n - 1 - i];
        buf[i] = hw_swap(b);
        buf[n - 1 - i] = hw_swap(a);
    }
}

uint32_t hw_sdio_read_scr(struct hw_sdio_card *sd) {
    TRY(hw_sdio_command(sd, SDIO_CMD_SET_BLOCKLEN, 8));
    TRY(hw_sdio_command(sd, SDIO_CMD_APP_CMD, sd->rca << 16));
    TRY(hw_sdio_command_read_data(sd, SDIO_ACMD_SEND_SCR, 0, sd->scr, 3));
    swap_buf(sd->scr, 2); // little endian for bitslice()
    return 0;
}

uint32_t hw_sdio_read_ssr(struct hw_sdio_card *sd) {
    TRY(hw_sdio_command(sd, SDIO_CMD_SET_BLOCKLEN, 64));
    TRY(hw_sdio_command(sd, SDIO_CMD_APP_CMD, sd->rca << 16));
    TRY(hw_sdio_command_read_data(sd, SDIO_ACMD_SEND_STATUS, 0, sd->ssr, 6));
    swap_buf(sd->ssr, 16); // little endian for bitslice()
    return 0;
}

uint32_t hw_sdio_erase(struct hw_sdio_card *sd, uint32_t start_lba, uint32_t end_lba) {
    uint32_t start_addr = hw_sdio_card_lba_to_addr(sd, start_lba);
    uint32_t end_addr   = hw_sdio_card_lba_to_addr(sd, end_lba);
    TRY(hw_sdio_command(sd, SDIO_CMD_ERASE_WR_BLK_START_ADDR, start_addr));
    TRY(hw_sdio_command(sd, SDIO_CMD_ERASE_WR_BLK_END_ADDR, end_addr));
    TRY(hw_sdio_command(sd, SDIO_CMD_ERASE, 0));
    return 0;
}


#define MAX_RETRIES 5

uint32_t hw_sdio_open(struct hw_sdio_card *sd) {

    // see RM 22.4.4
    memset(sd,0,sizeof(*sd));
    hw_sdio_reset(1); // turn on
    TRY(hw_sdio_command(sd, SDIO_CMD_GO_IDLE_STATE, 0));
    TRY(hw_sdio_command(sd, SDIO_CMD_SEND_IF_COND, 0x1aa));
    // We're a v2 card at least */
    sd->props = 1;
    if (!hw_sdio_command(sd, SDIO_CMD_IO_SEND_OP_COND, 0)) {
        return HW_SDIO_ERROR_BADCARD;
    }

    for (int tries = 0; tries < MAX_RETRIES; tries++) {
        TRY(hw_sdio_command(sd, SDIO_CMD_APP_CMD, 0)); // broadcast ACMD

        // Testing Card Busy, Voltage match, and capacity
        if (!(SDIO_STA_CCRCFAIL &
              hw_sdio_command(sd, SDIO_CMD_SD_APP_OP_COND, 0xc0100000))) {
            // Expect CCRCFAIL here.  Return an error code in the unused top 8 bits.
            return HW_SDIO_ERROR_OPCOND;
        }
        uint32_t resp1 = SDIO_RESP1;
        if ((resp1 & 0x80000000) != 0) {
            sd->ocr = resp1; // Ok OCR is valid
            break;
        }
        // Card is still powering up.  Wait before trying again.
        hw_busywait(10000*24);
    }

    TRY(hw_sdio_command(sd, SDIO_CMD_ALL_SEND_CID, 0));
    sd->cid[0] = SDIO_RESP1;
    sd->cid[1] = SDIO_RESP2;
    sd->cid[2] = SDIO_RESP3;
    sd->cid[3] = SDIO_RESP4;

    TRY(hw_sdio_command(sd, SDIO_CMD_SEND_RELATIVE_ADDRESS, 0));
    sd->rca = (SDIO_RESP1 >> 16) & 0xffff;
    if (! sd->rca) {

        /* If the card says '0' tell it to pick we assume this will
         * work because the previous send RCA worked and the card
         * should be in the ident state if it is functioning
         * correctly.
         */
        (void) hw_sdio_command(sd, SDIO_CMD_SEND_RELATIVE_ADDRESS, 0); // try again
        sd->rca = (SDIO_RESP1 >> 16) & 0xffff;
    }

    /* Save CSD in little endian order, i.e. bit 0 is bit 0 in word 0.  See bitslice() */
    TRY(hw_sdio_command(sd, SDIO_CMD_SEND_CSD, sd->rca << 16));
    sd->csd[3] = SDIO_RESP1;
    sd->csd[2] = SDIO_RESP2;
    sd->csd[1] = SDIO_RESP3;
    sd->csd[0] = SDIO_RESP4;

    TRY(hw_sdio_select(sd, sd->rca));
    TRY(hw_sdio_read_scr(sd)); // Capture the SCR
    TRY(hw_sdio_command(sd, SDIO_CMD_APP_CMD, sd->rca << 16));
    TRY(hw_sdio_command(sd, SDIO_CMD_SWITCH_FUNC, 2));

    hw_sdio_set_bus(HW_SDIO_4BIT, 18000000);
    // hw_sdio_set_bus(HW_SDIO_4BIT, 8000000);
    
    //hw_sdio_set_bus(HW_SDIO_4BIT, 24000000); // Doesn't work on powermcu board: DCRCFAIL

    sd->nb_units = 0;
    switch (hw_sdio_csd_structure(sd)) {
        // Size encoding depends on CSD structure version, see 5.3.2 CSD Register
    case 0: {
        uint32_t tmp_reg = ((1 << (hw_sdio_c_size_mult_v1(sd) + 2)) *
                            (1 <<  hw_sdio_read_bl_len(sd))) >> 9;
        sd->nb_units = tmp_reg * (hw_sdio_c_size_v1(sd) + 1);
        break;
    }
    case 1:
        sd->nb_units = (hw_sdio_c_size_v2(sd)+1) << 10;
        break;
    default:
        sd->nb_units = 0; // Bug if its not CSD V1 or V2
    }


    TRY(hw_sdio_read_ssr(sd));

    // if (DEBUG)
    infof("sd: %d MiB (%s)\n",
           (int)sd->nb_units/2048,
           hw_sdio_card_ccs(sd) ? "SDHC" : "SD"
        );
    infof("sd: cid: %08x %08x %08x %08x\n",
          sd->cid[0],
          sd->cid[1],
          sd->cid[2],
          sd->cid[3]);

    /* Reset the stats */
    memset(&sd->stats, 0, sizeof(sd->stats));

    return 0;

}


