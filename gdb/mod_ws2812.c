#ifndef MOD_WS2812
#define MOD_WS2812

/* FIXME: This needs to be built on top of HAL, and further abstracted
   into a proper module. */
#include <hw_stm32f103.h>
#include "pwm_bitstream.h"
#include "fixedpoint.h"

/* FIXME: This needs to be run into an external clock source. */
#define MS_PER_TICK 1
int video_sync(void) { return 0; }



/* LED STRIP DRIVER */


/* This uses a SPI peripheral.  The STM32F103 at 72MHz is to slow to
   bit-bang the 2.5MHz from interrupt, and busy wait is not an option
   due to other interrupts running.  So prepare the bit pattern in RAM
   and send it out in one go using DMA. */


//          rcc_gpio   rcc_spi   spi   rst       gpio    data clk master tx  hw_dma      ie  bits mode        div
// -----------------------------------------------------------------------------------------------------------------------------------------
const struct hw_spi hw_spi_tx8_master_0rw[] = {
    [1] = { RCC_GPIOA, RCC_SPI1, SPI1, RST_SPI1, GPIOA,  7,   5,  1,     1,  HW_DMA_1_3,  0,  8,  HW_SPI_0RW, SPI_CR1_BAUDRATE_FPCLK_DIV_32},
};

/* Note that SPI only allows power of two division on STM.  Different
   clocks would require external loopback of TIM pin and transmit in
   slave mode.  Two approaches seem to work:

   1. One SPI byte per WS2812B PWM token
      DIV_16 then gives 1.8us bit frame spacing which is within the 1.25 +- 0.6 ns in the spec

   2. One SPI bit per WS2818 bit
      DIV_32 gives 0.4us bit spacing, usable with bit encoding
      The latter uses a bit sequencer from pwm_bitstream.h
*/

/* Byte order G-R-B */


/* Data output is at A7.  FIXME: Clock is still at A5 but not used for
 * the LEDs. */
#define C_LED_SPI hw_spi_tx8_master_0rw[1]

#define LED_NB_BYTES 8*3
uint8_t led_dma_buf[1+8*LED_NB_BYTES] = {};

void led_send(const uint8_t *buf, uint32_t len) {
#if 0 // Old approach, one SPI byte per token
    if (len > LED_NB_BYTES) { len = LED_NB_BYTES; }
    for(int i=0; i<len; i++) {
        uint8_t b = buf[i];
        for (int j=0; j<8; j++) {
            led_dma_buf[i*8+j] =
                (b & (1<<(7-j))) ?
                0xF0 : // 1 = long pulse   11110000
                0xC0;  // 0 = short pulse  11000000
        }
    }
    uint32_t dma_bytes = len * 8 + 1;
    led_dma_buf[dma_bytes-1] = 0; // needed to ensure line goes low
#else // New approach: 3 SPI bits per token
    uint32_t dma_bytes = pwm_bitstream_write(led_dma_buf, buf, len*8);
#endif

    /* reset peripheral and start dma. */
    hw_spi_reset(C_LED_SPI);
    hw_spi_start(C_LED_SPI, led_dma_buf, dma_bytes);
}

int led_dma_done(void) {
    if (!hw_spi_ready(C_LED_SPI)) return 0;
    hw_spi_ack(C_LED_SPI);
    return 1;
}
volatile uint32_t spi1_count;
void spi1_isr(void) {
    spi1_count++;
}

struct led_config {
    uint32_t rate_mHz;
    uint8_t start_grb[3];
    uint8_t end_grb[3];
    uint8_t state;
};
const struct led_config *led_current;

/* This is a free running fractional counter.  Range below half are
   rising, range above are falling. */
uint32_t led_phase;
uint32_t led_phase_inc;
static inline uint32_t led_phase_fold(void) {
    if (led_phase < 0x80000000ULL) return led_phase << 1;
    return (~led_phase) << 1;
}
#define NB_PIXELS 8
uint8_t pixels[3*NB_PIXELS]; // configuration to be compiled to dma buffer
uint32_t pixel;      // current pixel for test pattern.

uint32_t led_nb_cycles;

void update_pixels(void) {
    if (!led_current) return;
    uint32_t frac = led_phase_fold();
    // LOG("frac = %x\n", frac);
    for(int j=0; j<NB_PIXELS; j++) {
        for(int i=0; i<3; i++) {
            uint32_t p =
                fixedpoint_mul(~frac, led_current->start_grb[i]) +
                fixedpoint_mul(frac,  led_current->end_grb[i]);
            pixels[i + 3*j] = p;
        }
    }
    led_phase += led_phase_inc;
}
void set_led(const struct led_config *config) {
    /* Period is specified in milliHz.  If that is set to 0 we need to
       map to 0.  So figure out the multiplier to convert
       1000*cycle/sec to inc/cycle.  Since this is always wrong on
       first attempt, annotate the units! */

    /* Here 'cycle' means the full LED cycle.  The 'phase' goes
       through one 32 bit overflow every cycle. */
    uint64_t inc_per_cycle = 0x100000000ULL;

    led_phase_inc =                      // inc          / frame
        (uint64_t)((inc_per_cycle     *  // inc          / cycle
                    config->rate_mHz  *  // 1000 * cycle / sec
                    MS_PER_TICK)         // 1000 * sec   / frame
                   / (1000 * 1000));     // 1            / 1000 * 1000
    //LOG("led_phase_inc = %x (%d)\n", led_phase_inc, led_phase_inc);
    led_current = config;

    if (config->state == 1) {
        led_nb_cycles++;
    }
};

void animation_poll(void) {
    if (video_sync()) {
        led_send(pixels, sizeof(pixels));

        if (led_current) {
            update_pixels();
        }
        else {
            // LED TEST PATTERN
            pixels[pixel]++;
            if (pixels[pixel] > 30) {
                pixels[pixel] = 0;
                pixel++;
                if (pixel >= sizeof(pixels)) {
                    pixel=0;
                }
            }
        }
    }
}

#endif
