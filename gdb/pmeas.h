#ifndef PMEAS_H
#define PEMAS_H

/* Period measurement. */
struct pmeas {
    uint32_t avg;  // (32-log_max) fractional bits
    uint32_t num;  // number of periods that were averaged
};

struct pmeas_state {
    /* Config */
    uint32_t log_max; // max = 1<<log_max

    /* User state */
    uint32_t write;       // low bit points to current measurement
    uint32_t read;        // ..
    struct pmeas meas[2]; // last masurement with (32-log_max) fractional bits

    /* For ISR only. */
    uint32_t num;   // number of measurements
    uint32_t accu;  // current sum
    uint32_t last_cc;

    /* For poll only */
    struct cbuf *b_wait;
    struct cbuf *b_reply;

};

static inline void pmeas_state_poll_write(volatile struct pmeas_state *p) {

    if (p->read != p->write) {
        // FIXME: make sure they are equal after incrementing.  The
        // buffer is only one deep so other variables are not valid.
        uint32_t read = ++p->read;
        uint32_t avg = p->meas[read&1].avg;
        uint32_t num = p->meas[read&1].num;
        (void)avg;
        //infof("period_avg: %d\n", avg);

        /* Poll the waiter queue. */
        struct cbuf *b_wait = p->b_wait;

        if (cbuf_bytes(b_wait)) {
            uint16_t len  = cbuf_get(b_wait);
            uint32_t have = cbuf_bytes(b_wait);
            if (len < have) {
                infof("bad measurement_wait size %d, %d. clearing.\n",
                      cbuf_bytes(b_wait), len);
                cbuf_clear(b_wait);
            }
            else {
                //infof("sending measurement reply: avg=%d, cont=%d\n", avg, len);
                uint8_t h[] = { U16_BE(TAG_REPLY) };
                uint8_t k[len]; cbuf_read(b_wait, k, len);
                uint8_t a[] = { U32_BE(avg), U32_BE(num) };
                CBUF_WRITE_3(p->b_reply, h, k, a);
            }
        }
    }
}


static inline void pmeas_update(volatile struct pmeas_state *p, uint32_t cc) {

    /* Current period measurement. */
    uint32_t meas = cc - p->last_cc;
    p->last_cc = cc;

    /* Update average.  The oscillator is quite unstable, so we need
       to measure for a long time if we want a good reading (e.g. for
       about a second).  */
    uint32_t accu = p->accu;
    uint32_t accu1 = accu + meas;
    uint32_t log_max = p->log_max;
    uint32_t max = 1 << log_max;
    if (accu1 < max) {
        p->num++;
        p->accu = accu1;
    }
    else {
        /* Double buffering is used to pass data to the low rate task.
           It can sync on p->write changing and pick up
           p->avg[p->write & 1]. */
        uint32_t write = p->write + 1;
        if (p->num > 0) {
            uint32_t avg = (accu << (32-log_max)) / p->num;
            p->meas[write & 1].avg = avg;
            p->meas[write & 1].num = p->num;
            p->write = write;
        }
        else {
            /* There was no previous measurement, so simply do not
               send it out.  Note that when the period is very low, it
               will take at least one period length for a
               measurement! */
        }
        p->num = 1;
        p->accu = meas;
    }

    /* Note that it took a while to realize that there are two
       problems to solve: calibration of static components, and
       compensation of temperature variations.  The temperature
       variations are slow, so we can use a fairly slow control
       algorithm for that, which allows us to focus on taking our time
       to get precise measurements. */
}


#endif
