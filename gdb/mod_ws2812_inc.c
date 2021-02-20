#ifndef MOD_WS2812
#define MOD_WS2812

/* LED STRIP DRIVER

   FIXME: This is a copy of mod_ws2812.c to stage an "incremental"
   version that sends each pixel individually to reduce the buffering.
   This will eventually be a drop-in replacement.

   SPI data pin update rate is 2.25 MBit/sec (/ 72.0 32)
   That runs at 3x the WS2812 PWM bit rate, which is 750kHz (/ 72000.0 (* 3 32))
   Which is 24x the BGR pixel rate, which is 31.25kHz (/ 72000.0 (* 3 32 24))

   STM is fast enough to handle the 31.25kHz pixel rate, which has (*
   24 3) 72 SPI bits, or exactly 9 SPI bytes.

   Using an interrupt routine avoids the need for the 3x bit packet
   buffer, and allows the pixels to be generated on the fly without a
   pixel buffer at all.


*/

/* FIXME: This needs to be built on top of HAL, and further abstracted
   into a proper module. */
#include <hw_stm32f103.h>
#include "pwm_bitstream.h"
#include "fixedpoint.h"


/* The STM32F103 at 72MHz is to slow to bit-bang the 2.5MHz from
   interrupt, and busy wait is not an option due to other interrupts
   running.  So we prepare the bit pattern in RAM and send it out in
   one go using SPI DMA. */

/* SPI DIV32 is approximate: / 72 32 is 2.25mHz */

#define SPI_CHUNK_DIV SPI_CR1_BAUDRATE_FPCLK_DIV_32
#define SPI_CHUNK_NEXT ledstrip_next
void ledstrip_next(void);
#include "mod_spi_chunk.c"


/* Note that SPI only allows power of two division on STM.  Different
   clocks would require external loopback of TIM pin and transmit in
   slave mode.  Two approaches seem to work:

   1. One SPI byte per WS2812B PWM token
      DIV_16 then gives 1.8us bit frame spacing which is within the 1.25 +- 0.6 ns in the spec

   2. One SPI bit per WS2818 bit
      DIV_32 gives 0.4us bit spacing, usable with bit encoding
      The latter uses a bit sequencer from pwm_bitstream.h

   I've removed the OLD one SPI byte per token.  See git history.
*/

/* Byte order G-R-B */

#ifndef LEDSTRIP_NB_LEDS
#error need LEDSTRIP_NB_LEDS
#endif

struct grb {
    uint8_t g,r,b;
} __attribute__((__packed__));

uint8_t ledstrip_dma_buf[PWM_BITSTREAM_NB_BYTES((LEDSTRIP_NB_LEDS)*24)];
//uint8_t ledstrip_dma_buf[400];

/* Invert the bitstream. */
#ifndef LEDSTRIP_INVERT
#define LEDSTRIP_INVERT 0
#endif


/* Remarks:

   (1) hw_spi_reset() glitches the data line, which is interpreted as
       a MSB=1 on green on the first pixel. To work around this, send
       a "boot screen" that has this first pixel as green so it
       doesn't show as bad, or quickly send a second frame.

   (2) we use only the DMA as a synchronization mechanism.  the ws2812
       frames are separated by 50us pauses (FIXME: which we should
       generate as SPI frames!).

*/





volatile uint32_t ledstrip_repeat;
volatile uint32_t ledstrip_dma_bytes;
void ledstrip_send(const struct grb *grb) {
    /* Create PWM bitstream. */
    ledstrip_dma_bytes =
        pwm_bitstream_write(
            ledstrip_dma_buf,
            (const uint8_t*)grb,
            LEDSTRIP_NB_LEDS * 24);
    // infof("ledstrip_send %d %d\n", dma_bytes, sizeof(ledstrip_dma_buf));
    ledstrip_repeat = 3;
    spi_chunk_send_dma(ledstrip_dma_buf, ledstrip_dma_bytes);
}

/* Similar, but use data producing callback to avoid double buffering. */
void ledstrip_send_gen(struct grb (*grb_gen)(void*, int led_nb), void *ctx, int nb_leds) {
    /* Create PWM bitstream. */
    struct bitbuf dst;
    bitbuf_init(&dst, ledstrip_dma_buf);
    for(uint32_t led=0; led<nb_leds; led++) {
        struct bitbuf src;
        struct grb grb = grb_gen(ctx, led);
        bitbuf_init(&src, (void*)&grb);
        for(uint32_t i=0; i<24; i++) {
            pwm_bitstream_bitbuf_write(&dst, bitbuf_read(&src));
        }
    }
    uint32_t dma_bytes = bitbuf_flush(&dst);
    //infof("ledstrip_send_gen %d %d\n", dma_bytes, sizeof(ledstrip_dma_buf));
    spi_chunk_send_dma(ledstrip_dma_buf, dma_bytes);
}


// called from DMA ISR in mod_spi_chunk.c
void ledstrip_next(void) {
    if (ledstrip_repeat--) {
        spi_chunk_send_dma(ledstrip_dma_buf, ledstrip_dma_bytes);
    }
}




struct ledstrip_config {
    uint32_t rate_mHz;
    uint32_t ms_per_tick;
    uint8_t start_grb[3];
    uint8_t end_grb[3];
    uint8_t state;
};

const struct ledstrip_config *ledstrip_current;

/* This is a free running fractional counter.  Range below half are
   rising, range above are falling. */
uint32_t ledstrip_phase;
uint32_t ledstrip_phase_inc;
static inline uint32_t ledstrip_phase_fold(void) {
    if (ledstrip_phase < 0x80000000ULL) return ledstrip_phase << 1;
    return (~ledstrip_phase) << 1;
}
#define NB_PIXELS 8
uint8_t pixels[3*NB_PIXELS]; // configuration to be compiled to dma buffer
uint32_t pixel;      // current pixel for test pattern.

uint32_t ledstrip_nb_cycles;

void update_pixels(void) {
    if (!ledstrip_current) return;
    uint32_t frac = ledstrip_phase_fold();
    // LOG("frac = %x\n", frac);
    for(int j=0; j<NB_PIXELS; j++) {
        for(int i=0; i<3; i++) {
            uint32_t p =
                fixedpoint_mul(~frac, ledstrip_current->start_grb[i]) +
                fixedpoint_mul(frac,  ledstrip_current->end_grb[i]);
            pixels[i + 3*j] = p;
        }
    }
    ledstrip_phase += ledstrip_phase_inc;
}
void set_led(const struct ledstrip_config *config) {
    /* Period is specified in milliHz.  If that is set to 0 we need to
       map to 0.  So figure out the multiplier to convert
       1000*cycle/sec to inc/cycle.  Since this is always wrong on
       first attempt, annotate the units! */

    /* Here 'cycle' means the full LED cycle.  The 'phase' goes
       through one 32 bit overflow every cycle. */
    uint64_t inc_per_cycle = 0x100000000ULL;

    ledstrip_phase_inc =                 // inc          / frame
        (uint64_t)((inc_per_cycle     *  // inc          / cycle
                    config->rate_mHz  *  // 1000 * cycle / sec
                    config->ms_per_tick) // 1000 * sec   / frame
                   / (1000 * 1000));     // 1            / 1000 * 1000
    //LOG("ledstrip_phase_inc = %x (%d)\n", ledstrip_phase_inc, ledstrip_phase_inc);
    ledstrip_current = config;

    if (config->state == 1) {
        ledstrip_nb_cycles++;
    }
};

#if 0
void ledstrip_animation_tick(void) {
    ledstrip_send(pixels, sizeof(pixels));

    if (ledstrip_current) {
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
#endif

#include "instance.h"
instance_status_t ledstrip_init(instance_init_t *i) {
    spi_chunk_init();
    return 0;
}
DEF_INSTANCE(ledstrip);
#endif