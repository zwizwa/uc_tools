#ifndef MOD_TIMEBASE_AV_C
#define MOD_TIMEBASE_AV_C


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


#endif
