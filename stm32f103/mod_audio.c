#ifndef MOD_AUDIO_C
#define MOD_AUDIO_C


#include "mod_timebase_av.c"


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

void audio_init(void) {
    /* Start audio driver. */
    hw_clockgen_init(C_AUDIO);
    hw_clockgen_arm(C_AUDIO);
    hw_clockgen_trigger(C_AUDIO);
}

#endif
