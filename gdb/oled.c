/* Simple A/V core test: oled display, pwm audio. */

#include "generic.h"
#include "base.h"
#include "gdbstub_api.h"
#include <string.h>
#include "playback.h"
#include "memory.h"
#include "gdbstub.h"
#include "pbuf.h"
#include "cbuf.h"
#include "sliplib.h"
#include "oled.h"

struct cbuf cbuf_from_usb; uint8_t cbuf_from_usb_buf[4];
struct cbuf cbuf_to_usb;   uint8_t cbuf_to_usb_buf[1024];

struct pbuf pbuf_to_dispatch; uint8_t pbuf_to_dispatch_buf[1024];


/* TIMEBASE */

#define AUDIO_DIV         1125  // giving 64kHz PWM rate, also PWM scale
#define VIDEO_DIV         1600  // giving 40 Hz Video frame rate
#define TICKS_PER_SEC     (72000000 / (AUDIO_DIV * VIDEO_DIV))
#define MS_PER_TICK       (1000 / TICKS_PER_SEC)
#define SAMPLE_RATE_KHZ   8
#define AUDIO_RATE_KHZ    (72000 / AUDIO_DIV)
#define SAMPLE_SPEED_24P8 (256 / (AUDIO_RATE_KHZ / SAMPLE_RATE_KHZ))
#define AUDIO_SCALE_24P8  AUDIO_DIV // 24.8 fixed point scale to full PWM scale

/* Above shows the time relations.
   Some asserts to document the actual values. */
CT_ASSERT(audio_rate,   64 == AUDIO_RATE_KHZ);
CT_ASSERT(ms_per_tick,  25 == MS_PER_TICK);
CT_ASSERT(ticks_hz,     40 == TICKS_PER_SEC);
CT_ASSERT(sample_speed, 32 == SAMPLE_SPEED_24P8);

/* Number of audio interrupts. */
volatile uint32_t audio_pulse;

/* Video sync semaphore-like read/write variables.  These are derived
 * from audio clock. */
volatile uint32_t video_pulse, video_handled;

/* Main "long time" counter, updated in video_sync handler to keep u64
   non-atomicity out of isr<->mainloop communication. */
uint64_t time_now;

static inline int video_sync(void) {
    uint32_t p = video_pulse;
    uint32_t h = video_handled;
    if (p != h) {
        video_handled = h + 1;
        time_now++;
        return 1;
    }
    else {
        return 0;
    }
}
static inline void update_timebase(void) {
    uint32_t a = audio_pulse+1;
    if (a == VIDEO_DIV) { a = 0; }
    if (!a) { video_pulse++; }
    audio_pulse = a;
}





/* AUDIO */


const struct hw_clockgen hw_audio_playback[] = {
//          rcc_tim   rcc_gpio   tim   gpio   pin div         duty          phase  pol chan     itr  irq (optional)
//-------------------------------------------------------------------------------------------------------------------
    [3] = { RCC_TIM3, RCC_GPIOB, TIM3, GPIOB, 0,  AUDIO_DIV,  AUDIO_DIV/2,  0,     1,  TIM_OC3, -1,  NVIC_TIM3_IRQ },

};
#define TIM_AUDIO 3
#define C_AUDIO hw_audio_playback[TIM_AUDIO]

volatile struct playback audio;


void HW_TIM_ISR(TIM_AUDIO)(void) {
    hw_clockgen_ack(C_AUDIO);
    struct playback a = audio; // local non-volatile copy
    uint32_t val = playback_next_li_8p8(&a);
    uint32_t val_scaled = AUDIO_SCALE_24P8 * val;
    val = (val_scaled >> 16);

    // Preload is enabled, so the value takes effect on the next pwm
    // cycle to avoid glitches.
    hw_clockgen_duty(C_AUDIO, val);
    audio.read = a.read; // update state

    /* Update software timebase for video and coarse timing. */
    update_timebase();
}

/* Use a simple concatenation mechanism for the samples.  This format
   is straightforward to write to Flash using the existing Erlang
   binary loader code. */
struct sample {
    uint8_t *buf;
    uint32_t len;
};
#define NB_SAMPLES 3
struct sample sample[NB_SAMPLES];

void scan_samples(uint8_t *store) {
    for(int i=0; i<NB_SAMPLES; i++) {
        uint32_t len = read_be(store, 4);
        if (len > 0x10000) {
            /* Safety check. */
            infof("sample %d too large: %d bytes\n", i, len);
            return;
        }
        infof("sample %d: %d bytes\n", i, len);
        sample[i].len = len;
        sample[i].buf = &store[4];
        store += 4 + len;
    }
}
void play_sample(uint32_t n) {
    if (n >= NB_SAMPLES) {
        infof("no sample %d\n", n); return;
    }
    // infof("sample %d: %x %d\n", n, sample[n].buf, sample[n].len);
    if ((!sample[n].buf) || (!sample[n].len)) {
        infof("bad sample %d\n", n); return;
    }
    playback_trigger(
        (struct playback *)  // volatile hack
        &audio,
        SAMPLE_SPEED_24P8,
        sample[n].len,
        sample[n].buf);
}


/* OLED */

//   A07 DATA
//   A05 CLK
//   A04 CS    (dedicated NSS, but we do it manually)
//   A03 DC
//   A02 RES

// see oled.h

uint8_t framebuffer[8*128];

/* Seems to work up to 36MHz even on dinky bread board with 5cm wires,
   but at that speed we have trouble feeding it fast enough so there
   doesn't seem to be a reason to go that high. */
#define OLED_DIV SPI_CR1_BAUDRATE_FPCLK_DIV_4 // 18 MHz
// #define OLED_DIV SPI_CR1_BAUDRATE_FPCLK_DIV_8 // 9 MHz
// #define OLED_DIV SPI_CR1_BAUDRATE_FPCLK_DIV_2 // 36 MHz

// The command squences involves the command pin and multiple page
// write commands.  Doesn't seem worth the effort to do it with DMA.


//          rcc_gpio   rcc_spi   spi   rst       gpio   out in  sck  mode        div
// -----------------------------------------------------------------------------------------------------------
const struct hw_spi_nodma hw_oled_spi[] = {
    [1] = { RCC_GPIOA, RCC_SPI1, SPI1, RST_SPI1, GPIOA,  7,  6,  5,  HW_SPI_1WR, OLED_DIV},
    [2] = { RCC_GPIOB, RCC_SPI2, SPI2, RST_SPI2, GPIOB, 15, 14, 13,  HW_SPI_1WR, OLED_DIV},
};

#define C_OLED_SPI hw_oled_spi[1]
#define OLED_CS  GPIOA,4
#define OLED_DC  GPIOA,3
#define OLED_RES GPIOA,2


static inline void oled_cs(int val) {
    hw_gpio_write(OLED_CS, val);
}
static inline void oled_dc(int val) {
    hw_gpio_write(OLED_DC, val);
}
static inline void oled_res(int val) {
    hw_gpio_write(OLED_RES, val);
}
/* Keep filling the tx buffer, and wait for TX end after last byte. */
static inline void oled_bytes_noflush(const uint8_t *b, uint32_t n) {
    while(n--) { hw_spi_nodma_wr(C_OLED_SPI, *b++); }
}
static inline void oled_fill_noflush(uint8_t b, uint32_t n) {
    while(n--) { hw_spi_nodma_wr(C_OLED_SPI, b); }
}
static inline void oled_byte_noflush(const uint8_t b) {
    oled_bytes_noflush(&b, 1);
}
static inline void oled_flush(void) {
    hw_spi_nodma_end(C_OLED_SPI);
}
#define CMD(b) oled_byte_noflush(b);
static inline void oled_init(void) {
    hw_spi_nodma_init(C_OLED_SPI);
    hw_gpio_write(OLED_CS, 1);
    hw_gpio_write(OLED_DC, 0);
    hw_gpio_write(OLED_RES, 1);
    hw_gpio_config(OLED_CS, HW_GPIO_CONFIG_OUTPUT);
    hw_gpio_config(OLED_DC, HW_GPIO_CONFIG_OUTPUT);
    hw_gpio_config(OLED_RES, HW_GPIO_CONFIG_OUTPUT);
    oled_res(0);
    hw_busywait_ms(10);
    oled_res(1);
    oled_dc(0);
    oled_cs(0);
    uint8_t contrast = 128;
    OLED_INIT(CMD,contrast);
    oled_flush();
    oled_cs(1);
    for (int i=0;i<sizeof(framebuffer);i++) {
        framebuffer[i] = i;
    }
}
void oled_send(void) {
    oled_cs(0);
    for (int p=0; p<8; p++) {
        oled_dc(0);
        OLED_PAGE(CMD, p, 0);
        oled_flush();
        oled_dc(1);
        oled_bytes_noflush(&framebuffer[p*128], 128);
        oled_flush();
    }
    oled_cs(1);
}
void oled_clear(void) {
    oled_cs(0);
    for (int p=0; p<8; p++) {
        oled_dc(0);
        OLED_PAGE(CMD, p, 0);
        oled_flush();
        oled_dc(1);
        oled_fill_noflush(0, 128);
        oled_flush();
    }
    oled_cs(1);
}
// https://raw.githubusercontent.com/adafruit/Adafruit-GFX-Library/master/glcdfont.c
const unsigned char font[] = {
    0x00, 0x00, 0x00, 0x00, 0x00, 0x3E, 0x5B, 0x4F, 0x5B, 0x3E, 0x3E, 0x6B,
    0x4F, 0x6B, 0x3E, 0x1C, 0x3E, 0x7C, 0x3E, 0x1C, 0x18, 0x3C, 0x7E, 0x3C,
    0x18, 0x1C, 0x57, 0x7D, 0x57, 0x1C, 0x1C, 0x5E, 0x7F, 0x5E, 0x1C, 0x00,
    0x18, 0x3C, 0x18, 0x00, 0xFF, 0xE7, 0xC3, 0xE7, 0xFF, 0x00, 0x18, 0x24,
    0x18, 0x00, 0xFF, 0xE7, 0xDB, 0xE7, 0xFF, 0x30, 0x48, 0x3A, 0x06, 0x0E,
    0x26, 0x29, 0x79, 0x29, 0x26, 0x40, 0x7F, 0x05, 0x05, 0x07, 0x40, 0x7F,
    0x05, 0x25, 0x3F, 0x5A, 0x3C, 0xE7, 0x3C, 0x5A, 0x7F, 0x3E, 0x1C, 0x1C,
    0x08, 0x08, 0x1C, 0x1C, 0x3E, 0x7F, 0x14, 0x22, 0x7F, 0x22, 0x14, 0x5F,
    0x5F, 0x00, 0x5F, 0x5F, 0x06, 0x09, 0x7F, 0x01, 0x7F, 0x00, 0x66, 0x89,
    0x95, 0x6A, 0x60, 0x60, 0x60, 0x60, 0x60, 0x94, 0xA2, 0xFF, 0xA2, 0x94,
    0x08, 0x04, 0x7E, 0x04, 0x08, 0x10, 0x20, 0x7E, 0x20, 0x10, 0x08, 0x08,
    0x2A, 0x1C, 0x08, 0x08, 0x1C, 0x2A, 0x08, 0x08, 0x1E, 0x10, 0x10, 0x10,
    0x10, 0x0C, 0x1E, 0x0C, 0x1E, 0x0C, 0x30, 0x38, 0x3E, 0x38, 0x30, 0x06,
    0x0E, 0x3E, 0x0E, 0x06, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x5F,
    0x00, 0x00, 0x00, 0x07, 0x00, 0x07, 0x00, 0x14, 0x7F, 0x14, 0x7F, 0x14,
    0x24, 0x2A, 0x7F, 0x2A, 0x12, 0x23, 0x13, 0x08, 0x64, 0x62, 0x36, 0x49,
    0x56, 0x20, 0x50, 0x00, 0x08, 0x07, 0x03, 0x00, 0x00, 0x1C, 0x22, 0x41,
    0x00, 0x00, 0x41, 0x22, 0x1C, 0x00, 0x2A, 0x1C, 0x7F, 0x1C, 0x2A, 0x08,
    0x08, 0x3E, 0x08, 0x08, 0x00, 0x80, 0x70, 0x30, 0x00, 0x08, 0x08, 0x08,
    0x08, 0x08, 0x00, 0x00, 0x60, 0x60, 0x00, 0x20, 0x10, 0x08, 0x04, 0x02,
    0x3E, 0x51, 0x49, 0x45, 0x3E, 0x00, 0x42, 0x7F, 0x40, 0x00, 0x72, 0x49,
    0x49, 0x49, 0x46, 0x21, 0x41, 0x49, 0x4D, 0x33, 0x18, 0x14, 0x12, 0x7F,
    0x10, 0x27, 0x45, 0x45, 0x45, 0x39, 0x3C, 0x4A, 0x49, 0x49, 0x31, 0x41,
    0x21, 0x11, 0x09, 0x07, 0x36, 0x49, 0x49, 0x49, 0x36, 0x46, 0x49, 0x49,
    0x29, 0x1E, 0x00, 0x00, 0x14, 0x00, 0x00, 0x00, 0x40, 0x34, 0x00, 0x00,
    0x00, 0x08, 0x14, 0x22, 0x41, 0x14, 0x14, 0x14, 0x14, 0x14, 0x00, 0x41,
    0x22, 0x14, 0x08, 0x02, 0x01, 0x59, 0x09, 0x06, 0x3E, 0x41, 0x5D, 0x59,
    0x4E, 0x7C, 0x12, 0x11, 0x12, 0x7C, 0x7F, 0x49, 0x49, 0x49, 0x36, 0x3E,
    0x41, 0x41, 0x41, 0x22, 0x7F, 0x41, 0x41, 0x41, 0x3E, 0x7F, 0x49, 0x49,
    0x49, 0x41, 0x7F, 0x09, 0x09, 0x09, 0x01, 0x3E, 0x41, 0x41, 0x51, 0x73,
    0x7F, 0x08, 0x08, 0x08, 0x7F, 0x00, 0x41, 0x7F, 0x41, 0x00, 0x20, 0x40,
    0x41, 0x3F, 0x01, 0x7F, 0x08, 0x14, 0x22, 0x41, 0x7F, 0x40, 0x40, 0x40,
    0x40, 0x7F, 0x02, 0x1C, 0x02, 0x7F, 0x7F, 0x04, 0x08, 0x10, 0x7F, 0x3E,
    0x41, 0x41, 0x41, 0x3E, 0x7F, 0x09, 0x09, 0x09, 0x06, 0x3E, 0x41, 0x51,
    0x21, 0x5E, 0x7F, 0x09, 0x19, 0x29, 0x46, 0x26, 0x49, 0x49, 0x49, 0x32,
    0x03, 0x01, 0x7F, 0x01, 0x03, 0x3F, 0x40, 0x40, 0x40, 0x3F, 0x1F, 0x20,
    0x40, 0x20, 0x1F, 0x3F, 0x40, 0x38, 0x40, 0x3F, 0x63, 0x14, 0x08, 0x14,
    0x63, 0x03, 0x04, 0x78, 0x04, 0x03, 0x61, 0x59, 0x49, 0x4D, 0x43, 0x00,
    0x7F, 0x41, 0x41, 0x41, 0x02, 0x04, 0x08, 0x10, 0x20, 0x00, 0x41, 0x41,
    0x41, 0x7F, 0x04, 0x02, 0x01, 0x02, 0x04, 0x40, 0x40, 0x40, 0x40, 0x40,
    0x00, 0x03, 0x07, 0x08, 0x00, 0x20, 0x54, 0x54, 0x78, 0x40, 0x7F, 0x28,
    0x44, 0x44, 0x38, 0x38, 0x44, 0x44, 0x44, 0x28, 0x38, 0x44, 0x44, 0x28,
    0x7F, 0x38, 0x54, 0x54, 0x54, 0x18, 0x00, 0x08, 0x7E, 0x09, 0x02, 0x18,
    0xA4, 0xA4, 0x9C, 0x78, 0x7F, 0x08, 0x04, 0x04, 0x78, 0x00, 0x44, 0x7D,
    0x40, 0x00, 0x20, 0x40, 0x40, 0x3D, 0x00, 0x7F, 0x10, 0x28, 0x44, 0x00,
    0x00, 0x41, 0x7F, 0x40, 0x00, 0x7C, 0x04, 0x78, 0x04, 0x78, 0x7C, 0x08,
    0x04, 0x04, 0x78, 0x38, 0x44, 0x44, 0x44, 0x38, 0xFC, 0x18, 0x24, 0x24,
    0x18, 0x18, 0x24, 0x24, 0x18, 0xFC, 0x7C, 0x08, 0x04, 0x04, 0x08, 0x48,
    0x54, 0x54, 0x54, 0x24, 0x04, 0x04, 0x3F, 0x44, 0x24, 0x3C, 0x40, 0x40,
    0x20, 0x7C, 0x1C, 0x20, 0x40, 0x20, 0x1C, 0x3C, 0x40, 0x30, 0x40, 0x3C,
    0x44, 0x28, 0x10, 0x28, 0x44, 0x4C, 0x90, 0x90, 0x90, 0x7C, 0x44, 0x64,
    0x54, 0x4C, 0x44, 0x00, 0x08, 0x36, 0x41, 0x00, 0x00, 0x00, 0x77, 0x00,
    0x00, 0x00, 0x41, 0x36, 0x08, 0x00, 0x02, 0x01, 0x02, 0x04, 0x02, 0x3C,
    0x26, 0x23, 0x26, 0x3C, 0x1E, 0xA1, 0xA1, 0x61, 0x12, 0x3A, 0x40, 0x40,
    0x20, 0x7A, 0x38, 0x54, 0x54, 0x55, 0x59, 0x21, 0x55, 0x55, 0x79, 0x41,
    0x22, 0x54, 0x54, 0x78, 0x42, // a-umlaut
    0x21, 0x55, 0x54, 0x78, 0x40, 0x20, 0x54, 0x55, 0x79, 0x40, 0x0C, 0x1E,
    0x52, 0x72, 0x12, 0x39, 0x55, 0x55, 0x55, 0x59, 0x39, 0x54, 0x54, 0x54,
    0x59, 0x39, 0x55, 0x54, 0x54, 0x58, 0x00, 0x00, 0x45, 0x7C, 0x41, 0x00,
    0x02, 0x45, 0x7D, 0x42, 0x00, 0x01, 0x45, 0x7C, 0x40, 0x7D, 0x12, 0x11,
    0x12, 0x7D, // A-umlaut
    0xF0, 0x28, 0x25, 0x28, 0xF0, 0x7C, 0x54, 0x55, 0x45, 0x00, 0x20, 0x54,
    0x54, 0x7C, 0x54, 0x7C, 0x0A, 0x09, 0x7F, 0x49, 0x32, 0x49, 0x49, 0x49,
    0x32, 0x3A, 0x44, 0x44, 0x44, 0x3A, // o-umlaut
    0x32, 0x4A, 0x48, 0x48, 0x30, 0x3A, 0x41, 0x41, 0x21, 0x7A, 0x3A, 0x42,
    0x40, 0x20, 0x78, 0x00, 0x9D, 0xA0, 0xA0, 0x7D, 0x3D, 0x42, 0x42, 0x42,
    0x3D, // O-umlaut
    0x3D, 0x40, 0x40, 0x40, 0x3D, 0x3C, 0x24, 0xFF, 0x24, 0x24, 0x48, 0x7E,
    0x49, 0x43, 0x66, 0x2B, 0x2F, 0xFC, 0x2F, 0x2B, 0xFF, 0x09, 0x29, 0xF6,
    0x20, 0xC0, 0x88, 0x7E, 0x09, 0x03, 0x20, 0x54, 0x54, 0x79, 0x41, 0x00,
    0x00, 0x44, 0x7D, 0x41, 0x30, 0x48, 0x48, 0x4A, 0x32, 0x38, 0x40, 0x40,
    0x22, 0x7A, 0x00, 0x7A, 0x0A, 0x0A, 0x72, 0x7D, 0x0D, 0x19, 0x31, 0x7D,
    0x26, 0x29, 0x29, 0x2F, 0x28, 0x26, 0x29, 0x29, 0x29, 0x26, 0x30, 0x48,
    0x4D, 0x40, 0x20, 0x38, 0x08, 0x08, 0x08, 0x08, 0x08, 0x08, 0x08, 0x08,
    0x38, 0x2F, 0x10, 0xC8, 0xAC, 0xBA, 0x2F, 0x10, 0x28, 0x34, 0xFA, 0x00,
    0x00, 0x7B, 0x00, 0x00, 0x08, 0x14, 0x2A, 0x14, 0x22, 0x22, 0x14, 0x2A,
    0x14, 0x08, 0x55, 0x00, 0x55, 0x00, 0x55, // #176 (25% block) missing in old
                                              // code
    0xAA, 0x55, 0xAA, 0x55, 0xAA,             // 50% block
    0xFF, 0x55, 0xFF, 0x55, 0xFF,             // 75% block
    0x00, 0x00, 0x00, 0xFF, 0x00, 0x10, 0x10, 0x10, 0xFF, 0x00, 0x14, 0x14,
    0x14, 0xFF, 0x00, 0x10, 0x10, 0xFF, 0x00, 0xFF, 0x10, 0x10, 0xF0, 0x10,
    0xF0, 0x14, 0x14, 0x14, 0xFC, 0x00, 0x14, 0x14, 0xF7, 0x00, 0xFF, 0x00,
    0x00, 0xFF, 0x00, 0xFF, 0x14, 0x14, 0xF4, 0x04, 0xFC, 0x14, 0x14, 0x17,
    0x10, 0x1F, 0x10, 0x10, 0x1F, 0x10, 0x1F, 0x14, 0x14, 0x14, 0x1F, 0x00,
    0x10, 0x10, 0x10, 0xF0, 0x00, 0x00, 0x00, 0x00, 0x1F, 0x10, 0x10, 0x10,
    0x10, 0x1F, 0x10, 0x10, 0x10, 0x10, 0xF0, 0x10, 0x00, 0x00, 0x00, 0xFF,
    0x10, 0x10, 0x10, 0x10, 0x10, 0x10, 0x10, 0x10, 0x10, 0xFF, 0x10, 0x00,
    0x00, 0x00, 0xFF, 0x14, 0x00, 0x00, 0xFF, 0x00, 0xFF, 0x00, 0x00, 0x1F,
    0x10, 0x17, 0x00, 0x00, 0xFC, 0x04, 0xF4, 0x14, 0x14, 0x17, 0x10, 0x17,
    0x14, 0x14, 0xF4, 0x04, 0xF4, 0x00, 0x00, 0xFF, 0x00, 0xF7, 0x14, 0x14,
    0x14, 0x14, 0x14, 0x14, 0x14, 0xF7, 0x00, 0xF7, 0x14, 0x14, 0x14, 0x17,
    0x14, 0x10, 0x10, 0x1F, 0x10, 0x1F, 0x14, 0x14, 0x14, 0xF4, 0x14, 0x10,
    0x10, 0xF0, 0x10, 0xF0, 0x00, 0x00, 0x1F, 0x10, 0x1F, 0x00, 0x00, 0x00,
    0x1F, 0x14, 0x00, 0x00, 0x00, 0xFC, 0x14, 0x00, 0x00, 0xF0, 0x10, 0xF0,
    0x10, 0x10, 0xFF, 0x10, 0xFF, 0x14, 0x14, 0x14, 0xFF, 0x14, 0x10, 0x10,
    0x10, 0x1F, 0x00, 0x00, 0x00, 0x00, 0xF0, 0x10, 0xFF, 0xFF, 0xFF, 0xFF,
    0xFF, 0xF0, 0xF0, 0xF0, 0xF0, 0xF0, 0xFF, 0xFF, 0xFF, 0x00, 0x00, 0x00,
    0x00, 0x00, 0xFF, 0xFF, 0x0F, 0x0F, 0x0F, 0x0F, 0x0F, 0x38, 0x44, 0x44,
    0x38, 0x44, 0xFC, 0x4A, 0x4A, 0x4A, 0x34, // sharp-s or beta
    0x7E, 0x02, 0x02, 0x06, 0x06, 0x02, 0x7E, 0x02, 0x7E, 0x02, 0x63, 0x55,
    0x49, 0x41, 0x63, 0x38, 0x44, 0x44, 0x3C, 0x04, 0x40, 0x7E, 0x20, 0x1E,
    0x20, 0x06, 0x02, 0x7E, 0x02, 0x02, 0x99, 0xA5, 0xE7, 0xA5, 0x99, 0x1C,
    0x2A, 0x49, 0x2A, 0x1C, 0x4C, 0x72, 0x01, 0x72, 0x4C, 0x30, 0x4A, 0x4D,
    0x4D, 0x30, 0x30, 0x48, 0x78, 0x48, 0x30, 0xBC, 0x62, 0x5A, 0x46, 0x3D,
    0x3E, 0x49, 0x49, 0x49, 0x00, 0x7E, 0x01, 0x01, 0x01, 0x7E, 0x2A, 0x2A,
    0x2A, 0x2A, 0x2A, 0x44, 0x44, 0x5F, 0x44, 0x44, 0x40, 0x51, 0x4A, 0x44,
    0x40, 0x40, 0x44, 0x4A, 0x51, 0x40, 0x00, 0x00, 0xFF, 0x01, 0x03, 0xE0,
    0x80, 0xFF, 0x00, 0x00, 0x08, 0x08, 0x6B, 0x6B, 0x08, 0x36, 0x12, 0x36,
    0x24, 0x36, 0x06, 0x0F, 0x09, 0x0F, 0x06, 0x00, 0x00, 0x18, 0x18, 0x00,
    0x00, 0x00, 0x10, 0x10, 0x00, 0x30, 0x40, 0xFF, 0x01, 0x01, 0x00, 0x1F,
    0x01, 0x01, 0x1E, 0x00, 0x19, 0x1D, 0x17, 0x12, 0x00, 0x3C, 0x3C, 0x3C,
    0x3C, 0x00, 0x00, 0x00, 0x00, 0x00 // #255 NBSP
};

static inline const uint8_t *font_char(int c) {
    return &font[5*c];
}
void oled_send_text(int page, int column, char *text) {
    oled_cs(0);
    oled_dc(0);
    OLED_PAGE(CMD, page, column);
    oled_flush();
    oled_dc(1);
    for(;*text;text++) {
        const uint8_t *fc = font_char(*text);
        oled_bytes_noflush(fc, 5);
        oled_fill_noflush(0, 1);
        oled_flush();
    }
    oled_cs(1);
}

void to_hex_u32(char *buf, uint32_t val) {
    const char hex[] = "0123456789abcdef";
    for (int i=0; i<8; i++) {
        buf[i] = hex[0xF & (val >> ((7-i)*4))];
    }
}

void animation_poll(void) {
    if (video_sync()) {
        //oled_send();
        oled_send_text(0,0,"hello friend");
        char buf[10] = {};
        to_hex_u32(buf, video_pulse); oled_send_text(1,10,buf);
        to_hex_u32(buf, audio_pulse); oled_send_text(2,10,buf);
    }
}


void usb_rx_dispatch(void *ctx, const struct pbuf *p) {
    if (p->count < 2) {
        infof("ignoring p->count=%d\n", p->count);
        return;
    }
    uint16_t tag = read_be(p->buf, 2);
    switch(tag) {
    case TAG_PING:
        cbuf_write_slip_tagged(&cbuf_to_usb, TAG_REPLY,
                               &p->buf[2], p->count-2);
        return;

    case TAG_GDB:
        // infof("tag_gdb: %d\n", p->count);
        _service.rsp_io.write(&p->buf[2], p->count-2);
        break;

    default:
        infof("bad tag %04x\n", tag);
    }
}

/* SLIP data incoming from USB controller.
   Called by USB driver.
   Packets end ip in dispatch() */
static void usb_rx(const uint8_t *buf, uint32_t len) {
    pbuf_slip_write(
        buf, len,
        &cbuf_from_usb,    // intermediate cbuf for slip data
        &pbuf_to_dispatch, // incoming tagged packet
        usb_rx_dispatch, NULL);
}


/* Drain bufer to USB controller.
   Called by USB subsystem when ready to transmit. */
static uint32_t usb_tx(uint8_t *buf, uint32_t room) {
    int rv;
    /* To respect message boundaries, slip_out is always read until it
       is empty before moving on to anything else. */
    if ((rv = cbuf_read(&cbuf_to_usb, buf, room))) return rv;
    /* Then only do one of these per buffer.  The main reason for that
       is to guarantee some minimal available space, but it might not
       be necessary. */
    if ((rv = slip_read_tagged(TAG_INFO,   info_read, buf, room))) return rv;
    //if ((rv = slip_read_tagged(TAG_PLUGIO, plugin_read, buf, room))) return rv;
    if ((rv = slip_read_tagged(TAG_GDB, _service.rsp_io.read, buf, room))) {
        // infof("read TAG_GDB %d\n", rv);
        return rv;
    }
    return 0;
}




/* ******** GDBSTUB GLUE */



const struct gdbstub_io app_io = {
    .write = usb_rx,
    .read  = usb_tx,
};


/* Bootloader starts in GDB RSP (GDBSTUB) mode.  If it sees a message
   that it cannot parse, it will call this function.  We install a new
   I/O handler on the USB ttyACM port. */
void switch_protocol(const uint8_t *buf, uint32_t size) {
    infof("Switch to SLIP\n");
    *_service.io = (struct gdbstub_io *)(&app_io);
    (*_service.io)->write(buf, size);
}


/* Called by bootloader before calling any other application code,
 * e.g. before calling switch_protocol()
 */


const char config_product[];


void start(void) {



    /* Set app interrupt vector table and initialize app memory. */
    hw_app_init();

    rcc_periph_clock_enable(RCC_GPIOA);
    rcc_periph_clock_enable(RCC_GPIOB);


    /* Buffer init */
    PBUF_INIT(pbuf_to_dispatch);
    CBUF_INIT(cbuf_to_usb);
    CBUF_INIT(cbuf_from_usb);

    /* Add main loop polling tasks. */
    _service.add(animation_poll);

    /* Start audio driver. */
    hw_clockgen_init(C_AUDIO);
    hw_clockgen_arm(C_AUDIO);
    hw_clockgen_trigger(C_AUDIO);

    /* OLED driver */
    oled_init();
    oled_clear();

    scan_samples((void*)0x8008000);
    play_sample(1);

    infof("BOOT1=%d\n", hw_gpio_read(GPIOB,2));
    infof("MS_PER_TICK=%d\n", MS_PER_TICK);
    infof("TIM_AUDIO: pri=%d\n", NVIC_IPR(C_AUDIO.irq));
    infof("\n");
    infof("product: %s\n",&config_product[0]);

}


/* For debug purposes.  Normally, code will run indefinitely. */

void stop(void) {
    hw_app_stop();
    _service.reset();
}



/* Application description for bootloader.  Stored in Flash at a fixed location. */
const char config_manufacturer[] CONFIG_DATA_SECTION = "Zwizwa";
const char config_product[]      CONFIG_DATA_SECTION = "OLED+PWM test";
const char config_firmware[]     CONFIG_DATA_SECTION = FIRMWARE;
const char config_version[]      CONFIG_DATA_SECTION = BUILD;
const char config_protocol[]     CONFIG_DATA_SECTION = "slip";

struct gdbstub_config config CONFIG_HEADER_SECTION = {
    .manufacturer    = config_manufacturer,
    .product         = config_product,
    .firmware        = config_firmware,
    .version         = config_version,
    .protocol        = config_protocol,
    .start           = start,
    .stop            = stop,
    .switch_protocol = switch_protocol,
};

