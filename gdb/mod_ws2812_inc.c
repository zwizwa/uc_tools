#ifndef MOD_WS2812
#define MOD_WS2812

/* LED STRIP DRIVER

   A fork of mod_ws2812.c to stage an "incremental" version that sends
   each pixel individually to reduce the buffering.  This will not be
   a drop-in replacement, but easy to change.

   SPI data pin update rate is 2.25 MBit/sec (/ 72.0 32)
   That runs at 3x the WS2812 PWM bit rate, which is 750kHz (/ 72000.0 (* 3 32))
   Which is 24x the BGR pixel rate, which is 31.25kHz (/ 72000.0 (* 3 32 24))

   STM is fast enough to handle the 31.25kHz pixel rate, which has (*
   24 3) 72 SPI bits, or exactly 9 SPI bytes.

   Using an interrupt routine avoids the need for the 3x bit packet
   buffer, and allows the pixels to be generated on the fly without a
   pixel buffer at all.

   A frame takes about 30us to send (/ (* 3 23) 2.25)

   On the scope, the inter-frame pixel time (the pixel "render time"
   from gbr to pwm waveform) is a little under 30us, so while fast
   enough, we can't really go much lower in response time to handle
   this and not end up producing end-of-frame signalling (50us pause).
   To avoid, precompute everything.

*/

/* FIXME: This needs to be built on top of HAL, and further abstracted
   into a proper module. */
#include <hw_stm32f103.h>
#include "pwm_bitstream.h"
#include "fixedpoint.h"
#include "seq_gen.h"


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

//uint8_t ledstrip_dma_buf[PWM_BITSTREAM_NB_BYTES((LEDSTRIP_NB_LEDS)*24)];
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
       frames are separated by 50us pauses.

       In most cases this is not necessary as updates are typically
       driven from a video frame clock and the whole system is fairly
       predictable giving plenty of inter-frame spaceing.

       However, to run at max frame rate, we could use the SPI to send
       the 50us blank time as 113 bits at 2.25MHz, or 15 bytes of
       zero.  This can be done using the exisiting 9 byte pixel wave
       buffer twice.  However in most cases this can be omitted as
       long as the frame update rate is low enough.


*/


/* 3 bytes, 24 data bits, with 3x PWM encoding gives 72 spi bits or 9
   spi bytes to hold the 110/100 pwm bit pattern for a single
   pixel. */

/* FIXME: One extra zero byte is reserved.  Otherwise the SPI data pin
   will produce a 1. */

#define LEDSTRIP_STATE_BUF_SIZE (9 + 1)
struct ledstrip_state {
    struct seq_gen *grb_gen;
    uint8_t buffers[2][LEDSTRIP_STATE_BUF_SIZE];
    uint32_t index;
} ledstrip_state;

/* Specialized bit pattern synthesizer that is a bit faster than
   pwm_bitstream.

   One byte is expanded to 3 bytes with the following pattern:

   1.01.01. 01.01.01 .01.01.0  bitstream
    7  6  5   4  3   2  1  0   bit number

   See assembly listing at the bottom.
*/

void ws2812_pwm_write_byte(uint8_t *buf, uint8_t val) {

    //                           1.01.01.
    buf[0] =                    0b10010010
        | ((val & 0b10000000) ? 0b01000000 : 0)
        | ((val & 0b01000000) ? 0b00001000 : 0)
        | ((val & 0b00100000) ? 0b00000001 : 0);

    //                           01.01.01
    buf[1] =                    0b01001001
        | ((val & 0b00010000) ? 0b00100000 : 0)
        | ((val & 0b00001000) ? 0b00000100 : 0);

    //                           .01.01.0
    buf[2] =                    0b00100100
        | ((val & 0b00000100) ? 0b10000000 : 0)
        | ((val & 0b00000010) ? 0b00010000 : 0)
        | ((val & 0b00000001) ? 0b00000010 : 0);

}
void ws2812_pwm_write_grb(uint8_t *buf, const struct grb *grb) {
    ws2812_pwm_write_byte(buf+0, grb->g);
    ws2812_pwm_write_byte(buf+3, grb->r);
    ws2812_pwm_write_byte(buf+6, grb->b);
}


/* Render GRB value into PWM waveform bitstream. */
void ledstrip_send_pixel(const struct grb *grb) {
    uint8_t *buf = ledstrip_state.buffers[ledstrip_state.index];
    /* Create PWM bitstream. */
#if 0
    /* Generic code. */
    pwm_bitstream_write_pad(
        buf,
        (const uint8_t*)grb,
        24 /* nb input bits, resulting in 72 output bits, or 9 bytes. */,
        0 /* pad with zero bits (there won't be any */);
#else
    /* More efficient manual code. */
    ws2812_pwm_write_grb(buf, grb);
#endif

    /* Send a trailing zero byte to make sure that the data line goes
       to zero.  I've tried a lot of different things, but a full zero
       byte is the only thing that works to properly terminate the bit
       sequence and not leave it hanging high. */
    buf[LEDSTRIP_STATE_BUF_SIZE-1] = 0;

    spi_chunk_send_dma(buf, LEDSTRIP_STATE_BUF_SIZE);
}


/* Read from buffer. */

/* The DMA needs to be fed one pixel at a time.  Since we would need
   to keep track of a buffer and count anyway, it seems better to push
   the bookkeeping towards the user and impose a generator API. */


void ledstrip_send(struct seq_gen *grb_gen) {
    ledstrip_state.grb_gen = grb_gen;
    ledstrip_next();
}
void ledstrip_next(void) {
    struct seq_gen *g = ledstrip_state.grb_gen;
    if (g) {
        /* Generator is active.  Get the next pixel. */
        const struct grb *grb = g->pop(g);
        if (!grb) {
            /* Done.  This would be a place to send the postamble,
               e.g. the 50us spacer.  But we don't really need that in
               practice so just stop here. */
            ledstrip_state.grb_gen = 0;
        }
        else {
            //hw_busywait_us(50);
            ledstrip_send_pixel(grb);
            //infof("(%d,%d,%d)\n", grb->r, grb->g, grb->b);
        }
    }
}

/* Generator wrapper for flat pixel buffer. */
struct grb_buf_gen {
    struct seq_gen gen;
    const struct grb *grb;
    uint16_t index, len;
};
const void *grb_buf_gen_pop(struct seq_gen *_g) {
    struct grb_buf_gen *g = (void*)_g;
    if (g->index >= g->len) return NULL;
    return &g->grb[g->index++];
}
void grb_buf_gen_init(struct grb_buf_gen *g, const struct grb *grb_buf, uint16_t len) {
    g->gen.pop = grb_buf_gen_pop;
    g->grb = grb_buf;
    g->index = 0;
    g->len = len;
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



/*
08003176 <ws2812_pwm_write_byte>:
 8003176:	f011 0f80 	tst.w	r1, #128	; 0x80
 800317a:	b510      	push	{r4, lr}
 800317c:	f3c1 1340 	ubfx	r3, r1, #5, #1
 8003180:	bf0c      	ite	eq
 8003182:	2492      	moveq	r4, #146	; 0x92
 8003184:	24d2      	movne	r4, #210	; 0xd2
 8003186:	f011 0f40 	tst.w	r1, #64	; 0x40
 800318a:	bf0c      	ite	eq
 800318c:	2200      	moveq	r2, #0
 800318e:	2208      	movne	r2, #8
 8003190:	4323      	orrs	r3, r4
 8003192:	4313      	orrs	r3, r2
 8003194:	f011 0f10 	tst.w	r1, #16
 8003198:	7003      	strb	r3, [r0, #0]
 800319a:	bf0c      	ite	eq
 800319c:	2349      	moveq	r3, #73	; 0x49
 800319e:	2369      	movne	r3, #105	; 0x69
 80031a0:	f011 0f08 	tst.w	r1, #8
 80031a4:	bf0c      	ite	eq
 80031a6:	2200      	moveq	r2, #0
 80031a8:	2204      	movne	r2, #4
 80031aa:	4313      	orrs	r3, r2
 80031ac:	f011 0f04 	tst.w	r1, #4
 80031b0:	7043      	strb	r3, [r0, #1]
 80031b2:	bf0c      	ite	eq
 80031b4:	2324      	moveq	r3, #36	; 0x24
 80031b6:	23a4      	movne	r3, #164	; 0xa4
 80031b8:	f011 0f02 	tst.w	r1, #2
 80031bc:	bf0c      	ite	eq
 80031be:	2200      	moveq	r2, #0
 80031c0:	2210      	movne	r2, #16
 80031c2:	f011 0f01 	tst.w	r1, #1
 80031c6:	ea42 0303 	orr.w	r3, r2, r3
 80031ca:	bf0c      	ite	eq
 80031cc:	2200      	moveq	r2, #0
 80031ce:	2202      	movne	r2, #2
 80031d0:	4313      	orrs	r3, r2
 80031d2:	7083      	strb	r3, [r0, #2]
 80031d4:	bd10      	pop	{r4, pc}
*/
