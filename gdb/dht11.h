#ifndef DHT11_H
#define DHT11_H

/* DHT11 driver

   Check online for a timing diagram.
   http://www.ocfreaks.com/basics-interfacing-dht11-dht22-humidity-temperature-sensor-mcu/

   All descriptions I've found say "data bit starts with 50us low".
   I'm rotating that loop such that the 80/50us low time is trailing
   the information carrying high-time.  This removes special cases in
   the parsing.

   MCU:
     - low
     - wait 18000
     - high
     - init receiver
   DHT11 START:
     - wait 20-40
     - low
     - wait 80
     - high
     - wait 80
     - low
     - wait 50
     - high

   DHT11 TOKEN:
     - wait 80:ACK / 25:BIT0 / 70:BIT1
     - write 0
     - wait 80:ACK / 50:BITx
     - write 1

   The parsing state machine then looks quite simple:

   init:
     - reset timer
   update:
     - 1->0: capture edge time, write to queue
     - 0->1: reset timer

   It is initialized when the micro releases the line at the end of
   the response cycle.  The first 1->0 transition it sees is the
   response time of the DHT11.  The second 1->0 transition is the 80us
   acknowledgement.  After that, each 1->0 transition is information
   carrying.  So the state machine could be initialized at count = -1,
   and only write bits when count >= 1.
*/


/* Platform dependent interface.  See dht11.c for STM32F103 example. */
static inline void dht11_hw_reset_timer(void);
static inline void dht11_hw_stop_timer(void);
static inline int dht11_hw_read_timer_bit(struct dht11 *s);
static inline void dht11_hw_enable_interrupt(void);
static inline void dht11_hw_disable_interrupt(void);

struct dht11 {
    uint32_t count;
    uint8_t data[5];
};
static inline void dht11_write_bit(struct dht11 *s, int val) {
    int byte = (s->count / 8);
    int bit  = (s->count % 8);
    s->data[byte] |= (val << bit);
}
/* Platform independent functionality */
static inline void dht11_init(struct dht11 *s) {
    /* Ignore the first two negedges.  They are part of the initial
     * response of the DHT11. */
    s->count = -2;
    memset(s->data, 0, sizeof(s->data));
    dht11_hw_reset_timer();
    dht11_hw_enable_interrupt();
}
static inline void dht11_posedge(struct dht11 *s, int edge) {
    if (s->count < DHT11_NB_DATA_BITS) {
        dht11_hw_reset_timer();
        dht11_hw_disable_interrupt();
    }
    else {
        dht11_hw_stop_timer();
        /* Message is ready.  Perform check. */
        uint8_t cs = 0;
        for (int i=0; i<4; i++) cs += s->data[i];
        if (cs != s->data[4]) {
            /* Bad checksum */
        }
        else {
            uint8_t rh = data[0];
            uint8_t t  = data[2];
            // FIXME write it to queue
        }
    }
}
static inline void dht11_negedge(struct dht11 *s, int edge) {
    /* The information is stored in the elapsed time since last
     * posedge.  We write it into the bit queue if it is a data
     * bit. */
    int bitval = dht11_hw_read_timer_bit(s);
    s->count++;
    if (s->count > 0) {
        dht11_write_bit(s, bitval);
    }
}


#endif
