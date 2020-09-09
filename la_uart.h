/* Logic Analyzer: UART */

/* There are Rust and C++ counterparts to this.  However, I am
   currently in the process of changing the implementation a bit to
   allow for better integration into an existing system:

   - Analyzer code should probably run on STM32F103 later, e.g. EXTI
     ISR + timestamps + RLE encoded protocol on USB.

   - Make it easier to make custom analyzers by just duplicating code
     and changing it.  E.g. do not include too much configurability if
     it makes the code more complicated.  This means that replacing a
     number by a variable is ok, but don't add extra case clauses.

   - I have come to realize that polyglot projects are a real pain to
     manage.  My current project is based in C and LuaJIT, and for now
     it seems best to not introduce the extra dependencies (Rust for
     Logan, or Python/C++ for pyla).  The ideas are simple enough so
     they can just be transliterated.

   - Parsers need to (eventually) operate on differential data.  This
     seems to require two operations:

     - Wind forward in time and get value

     - Wind to next event and measure time diff

   - Parsers are written in "push style", presented with one event at
     a time, exposed as a static inline function to allow inlining
     when composing machines.  This seems much more convenient than
     pull-style, and for lower rate processing I already have a
     push-to-actor converter in Lua and Erlang.

   - Also keeping in mind to do in-place processing to simplify memory
     management and increase data locality for speed.  This probably
     means that analyzers have to be generated and specialized to
     task.

   So for the C side this is mostly a collection of static inline
   functions in push-style (e.g. CPS).

*/

#include "macros.h"
#include <stdint.h>

/* The data structures should be such that the inner loop can be
   easily inlined and optimized.  The rest doesn't matter a whole lot. */

/* All protocol analysis will probably happen on 64 bit host, so data
   structures should be designed to be native.  Let's stick to time as
   uintptr_t,.  On 32-bit this is 71 minutes @ 1MHz on 32-bit which is
   still plenty of time. */
typedef uintptr_t la_time_t;

/* An event is a data sample in time.  To simplify, we currently keep
   this untyped, and use a machine word to contain a sample. */
struct la_event {
    la_time_t time;
    uintptr_t value;
};

/* Events are passed in push-style to another processor.  Abstractly,
   a processor is a thing with private context that takes a read-only
   event. */
struct la;
typedef void (*la_push_f)(struct la *, const struct la_event *);
struct la {
    /* Push data into this la. */
    la_push_f push;
};


/* For RLE representation on uC, a bit-packet representation is
   probably more appropriate, e.g. 1 bit value + 15 bit relative time,
   or 2 bit value + 14. bit relative time.  Let's not worry about that
   yet and first create an inner loop. */

/* A UART parser */

/* Config is kept separate to make it easier to specify this as
 * const. */

struct la_uart_config {
    uintptr_t channel;
    uintptr_t clock_div;
    uintptr_t bit_stop;
    intptr_t bit_parity;   // negative is no parity
    /* Analyzed data is pushed here. */
    struct la *out;
};

struct la_uart {
    struct la la;
    /* The idea is to keep the config such that the compiler can
       easily inline it.  Not entirely clear how to do that.  So let's
       just make it work first. */
    const struct la_uart_config *config;

    /* State */
    uintptr_t state;
    uintptr_t bits_data;
    uintptr_t bits_count;
    uintptr_t bits_parity;
    uintptr_t delay;

};

/* State */
#define LA_UART_IDLE   0
#define LA_UART_BREAK  1
#define LA_UART_SAMPLE 2

/* Parser is adapted from pyla and rewritten to use the push-style
   event encoding. */

/* FIXME: This is an inner loop that expects exactly one input sample
   per time step.  Change it to allow differential encoding. */

#define LOG_DBG(...)
//#define LOG_DBG LOG

INLINE void la_uart_push(struct la_uart *s,
                         const struct la_event *in) {

    const struct la_uart_config *c = s->config;

    int bit = (in->value >> c->channel) & 1;
    LOG_DBG("%d", bit);
    switch (s->state) {

    case LA_UART_IDLE:
        LOG_DBG("I");
        if (bit) break; // still idle

        // Skip start bit and sample first bit in the middle.
        s->state = LA_UART_SAMPLE;
        s->bits_data = 0;
        s->bits_count = 0;
        s->bits_parity = 0;
        s->delay = c->clock_div + (c->clock_div >> 1) - 1;
        break;

    case LA_UART_BREAK:
        LOG_DBG("B");
        if (!bit) break; // still break
        s->state = LA_UART_IDLE;
        break;

    case LA_UART_SAMPLE:
        LOG_DBG("D");
        if (s->delay > 0) {
            s->delay--;
        }
        else {
            if (s->bits_count == c->bit_stop) {
                struct la_event e = {
                    .time  = in->time,
                    .value = s->bits_data,
                };
                if (!bit) {
                    LOG_DBG("\n[F]");
                    /* Use the location of the stop bit as the
                       location of the frame error bit. */
                    e.value |= (1 << c->bit_stop);
                    s->state = LA_UART_BREAK;
                }
                else {
                    s->state = LA_UART_IDLE;
                }
                struct la *out = s->config->out;
                out->push(out, &e);
            }
            else if (s->bits_count == c->bit_parity) {
                if (bit != s->bits_parity) {
                    LOG_DBG("\n[P]");
                    s->bits_data |= (1 << c->bit_parity);
                }
            }
            else { // DATA BIT
                // Store and and schedule next sample.
                // LOG_DBG("%c", bit ? 'x' : '_');
                LOG_DBG("\n[%d]", bit);
                s->bits_parity ^= bit;
                s->bits_data |= (bit & 1) << s->bits_count;
            }
            s->bits_count++;
            s->delay = c->clock_div-1;
        }
        break;

    default:
        // NOT REACHED
        break;
    }
}
