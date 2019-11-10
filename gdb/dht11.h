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


/* DESIGN

   This tests a new state machine design approach based on the
   following principles:

   - Capture the functionality inside a start function that will be
     called by the highlevel code, and an event handler function that
     will be called by system support.

   - Keep only a single event handler function, and abstract events
     through data structures.  This seems to keep control flow a bit
     more clear: the start function asks for events, and each call of
     the handle function might ask for more events.

   - The "holes" for system _hw_ functionality should be implemented
     as inline functions.  This makes better C compiler optimizations
     possible even when coupling between hw and the logic is quite
     tight.

*/


#include <stdint.h>
#include <string.h>

struct dht11 {
    int32_t phase;  // request sequencing
    int32_t count;  // response bit count
    uint8_t data[5];
};


/* SYSTEM INTERFACE
   See dht11.c for STM32F103 example. */

/* Delivery of validated response. */
static inline void dht11_hw_response(struct dht11 *, uint8_t rh, uint8_t t);


/* IO access */
static inline void dht11_hw_io_write(struct dht11 *, int val);

/* Time measurement
   start:   start counting at 0
   us:      return time elapsed in us since reset
   stop:    done with device
*/
static inline void dht11_hw_time_reset(struct dht11 *);
static inline uint32_t dht11_hw_time_us(struct dht11 *);


/* IO event interface:
   enable:  we want POSEDGE/NEGEDGE events.
   disable: done, disable hardware */
#define DHT11_EVENT_POSEDGE 0
#define DHT11_EVENT_NEGEDGE 1
static inline void dht11_hw_io_enable(struct dht11 *);

/* Time delay events:
   start:  we want a DELAY event after us microseconds
   stop:   disable events
*/
#define DHT11_EVENT_DELAY 2
static inline void dht11_hw_delay_start(struct dht11 *, uint32_t us);





/* Event handler

   The machine is completely asynchronous.
   System will need to call this when events (interrupts) come in.

   Note on desing: I've started to appreciate the single event handler
   function instead of providing multiple C functions.  Somehow
   keeping the async part contained makes it more readable, e.g. it is
   possible to easily see a "goto" that spans across events.

*/

static inline void dht11_request(struct dht11 *s) {
    memset(s, 0, sizeof(*s));
    /* Start request pulse on the I/O line. */
    dht11_hw_io_write(s, 0);
    /* Request DHT11_EVENT_DELAY */
    s->phase = 1;
    dht11_hw_delay_start(s, 18000);
}

static inline void dht11_handle(struct dht11 *s, uint32_t event) {
    switch (event) {

    case DHT11_EVENT_DELAY:
        /* Timer will sequence the request procedure. */
        switch (s->phase) {
        case 1:
            /* End request pulse on the I/O line */
            dht11_hw_io_write(s, 0);
            s->phase++;

            /* Start following edges, but skip the first two that are
               part of the preamble. */
            s->count = -2;
            dht11_hw_io_enable(s);
            dht11_hw_time_reset(s);

            /* Register a timeout */
            dht11_hw_delay_start(s, 1000000);
            break;

        case 2:
            /* Communication should be done by now. */
            /* Ensure spurious events are ignored. */
            s->phase++;
            break;

        default:
            /* INVALID */
            break;
        }

    case DHT11_EVENT_NEGEDGE:
        /* A bit is encodued in the elapsed time since last
         * posedge. Decode by thresholding on average of on/off
         * times. */
        if ((s->count >= 0) && (s->count < 40)) {
            uint32_t us = dht11_hw_time_us(s);
            int bitval = us < ((24+72)/2);
            int byte = (s->count / 8);
            int bit  = 7 - (s->count % 8);  // MSB first
            s->data[byte] |= (bitval << bit);
        }
        else {
            /* Ignore edges that are part of preamble, or any trailing
             * spurious edges. */
        }
        s->count++;
        break;


    case DHT11_EVENT_POSEDGE:
        if (s->count == sizeof(s->data) * 8) {
            /* Deliver only when consistency check passes. */
            uint8_t cs = 0;
            for (int i=0; i<4; i++) cs += s->data[i];
            if (cs == s->data[4]) {
                uint8_t rh = s->data[0];
                uint8_t t  = s->data[2];
                dht11_hw_response(s, rh, t);
            }
            else {
                /* BAD CHECKSUM */
            }
        }
        else {
            /* There's more to come.  Reset timer to measure the time
             * to next negedge. */
            dht11_hw_time_reset(s);
        }
        break;
    default:
        /* INVALID */
        break;
    }
}


#endif