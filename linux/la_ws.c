/* Logic Analyzer Websocket interface

   This takes an 8x1 stream on stdin (e.g. what comes out of the
   Saleae Logic8), pushes it into a circular buffer, keeps track of a
   mipmap representation, and provides access to the data via a web
   interface.

   The JavaScript part is found elsewhere.
*/


/* View and model updates need to be mutually exclusive. */
#include "os_thread.h"
os_mutex_t minmax_lock;
#define MINMAX_EXCLUSIVE() \
    WITH_PRE_POST(&minmax_lock, os_mutex_lock, os_mutex_unlock)

/* Startup sequence:
   - the elf generated from this file starts first
   - it opens a TCP socket with a web server where browser can connect
   - this serves WEBSERVER_INDEX_HTML as root document
   - which starts la_ws.js to set up a web socket and basic ui. */
#define WEBSERVER_INDEX_HTML "la_ws.html"

//#define MMAP_FILE_LOG LOG
#include "mod_minmax_8x1.c"
#include "mod_minmax_viewmodel.c"
#include "mod_webserver.c"
#include "mod_websocket_leb128s.c"

#include "assert_execvp.h"


OS_THREAD_STACK(http_thread, 1024);
uint16_t tcp_port = 0;
OS_THREAD_MAIN(http_loop, ctx) {
    webserver_loop(tcp_port);
    return 0;
}

/* Once slice is currently 1MB, same as Saleae C++ code buffer size.
   Samplerate is currently 2Msps
   So once slice is half a second.
   30 seconds is probably enough. */

uintptr_t nb_slices;

/* Default is stdin/stout */
int in_fd=0, out_fd=1;

/* High level events.  See comments in push_sample().

   Use a circular stack to record events.  Note that we only use it as
   a delay register containing the last couple of events.  The reader
   will index elements and mark them, but will never delete anything.
*/

/* Only 2 trigger events are needed: a completed one (that has passed
   its hold time), and a possibly in-progress one. */
typedef struct {
    uintptr_t time;
    int handled:1;
} event_element_t;
typedef struct {
    uintptr_t top;
    event_element_t stack[2];
} event_stack_t;
event_stack_t event_stack = {};

#define NS(name) event##name
#include "ns_cstack.h"
#undef NS

/* FIXME: we don't need to special-case the "there was no trigger yet"
   case if we initialize the queue with fake, handled events with
   proper time stamps. */
struct analyzer {
    event_stack_t stack;
    /* State. */
    uint8_t last_sample;
    /* Config.  Note that holdoff is clipped to screen_post_trigger. */
    uintptr_t holdoff;
};
struct analyzer analyzer;


// Move this elsewhere
#if 0
    /* Stretch the holdoff to be at least the time span between the
       location of the trigger point on screen, and the right edge of
       the screen. */
    uintptr_t effective_holdoff =
        (s->holdoff < s->screen_post_trigger) ?
        s->screen_post_trigger : s->holdoff;
#endif


/* Triiggered waveform display is not trivial.  I approached this the
   wrong way initially, not realizing that we need to use properties
   of the waveform _and_ the view to compute screen updates.

   There are 3 time instances that are important:

   t_trig  is the trigger time stamp, e.g. caused by a transition
   t_hold  is the time >t_trig after which a new trigger can happen
   t_disp  is the time >t_trig representing the edge of the screen

   We assume that t_hold >= t_disp, i.e. for display triggering it
   doesn't make sense to re-trigger before reaching the end of the
   screen.  This condition is maintained _outside_ of push_samples().

   For the main loop, do as little work as possible on every
   iteration. A large part of this is to not perform indirect accesses
   and only do it once an event is detected.

   Events are computed after the main loop.  It's ok to use that
   granularity as long as the loop runs across the minimal buffer
   size to not introduce more delay at the event level.
*/



/* Data processing. */
static inline uintptr_t push_samples(
    struct analyzer *s,
    const uint8_t *buf, uintptr_t nb_samples, uintptr_t time) {

    uintptr_t t_hold = event_index(&s->stack, 0)->time + s->holdoff;
    intptr_t  t_hold_diff = time - t_hold;
    uintptr_t last_sample = s->last_sample;

    for(uintptr_t i=0; i<nb_samples; i++) {
        uint8_t sample = buf[i];
        if (t_hold_diff >= 0) {
            uint8_t falling_edge_events =
                (~sample) && (sample ^ last_sample);

            if (1 & falling_edge_events) {

                /* Hold time expired + edge detected.  This is a
                   proper trigger event.  Record it. */
                event_element_t e = { .time = time, .handled = 0 };
                event_push(&s->stack, e);

                /* Recompute loop variables. */
                t_hold_diff = s->holdoff;
            }
        }
        else {
            /* No data needs to be accessed if the holdoff time has
               not yet expired. */

            /* FIXME: Optimization: we don't actually need to read the
               input data until the very last sample, so could just
               skip ahead i and t_diff here. */
        }
        last_sample = sample;
        t_hold_diff--;
    }
    s->last_sample = last_sample;
    return time;
}

struct os_tcp_socket static_socket;

static inline void send_events(
    struct analyzer *s, uintptr_t t_endx, uintptr_t post_trigger_samples) {

    /* Go over all events, and pick the most recent one that is
       complete and not yet sent.  Mark that one and everything before
       as sent. */
    for (uintptr_t i=0; i<ARRAY_SIZE(analyzer.stack.stack); i++) {
        event_element_t *e = event_index(&analyzer.stack, i);

        if (e->handled) {
            /* Event was handled before.  We don't need to look at
               anything older. */
            break;
        }

        if (e->time + post_trigger_samples <= t_endx) {
            /* Send out event only if all the post-trigger data has
               arrived. */
            e->handled = 1;
            struct blocking_io *ws_io = NULL; // FIXME
            if (ws_io) {
                struct tag_u32 msg = {
                    .reply_ctx = ws_io,
                };
                send_tag_u32(&msg);
            }
            break;
        }

        /* Try next */
    }
}

int la_loop(void) {
    uintptr_t time = 0;
    uint8_t *circ_buf = map.level[0].buf;
    uintptr_t slice_nb = 0;
    uintptr_t slice_size = MINMAX_SLICE_SIZE;
    for(;;) {
        LOG("\r%03d", slice_nb);
        uint8_t *buf = circ_buf + slice_size * slice_nb;
        /* Note that we are writing straight into the buffer without
           locking.  If gui requests level 0 data it might get new or
           old. We can't just put the read in a critical section
           because then there is no time slot left for the view.
           However, only the data changes, and none of the indexing,
           so the worst that can happen is display glitches.  Deal
           with it later. */
        assert_read_fixed(in_fd, buf, slice_size);

        time = push_samples(&analyzer, buf, slice_size, time);

        send_events(&analyzer, time, 123 /* FIXME */);

        minmax_update_slice(&map, slice_nb);
        slice_nb = (slice_nb + 1) % nb_slices;
    }
    return 0;
}



// ws = new WebSocket("ws://10.1.3.29:3456");
// see test_websocket_timeseries.sh

int main(int argc, char **argv) {
    if (argc < 5) {
        LOG("usage: %s <documentroot> <buffer> <nb_slices> <tcp_port> [<stream_program>]\n", argv[0]);
        return 1;
    }
    if (argc > 5) {
        const char *program = argv[5];
        const char *argv[] = {program, NULL};
        assert_fork_execvp(&in_fd, &out_fd, argv, NULL);
        LOG("started %s\n", program);
    }

    nb_slices = atoi(argv[3]);
    tcp_port = atoi(argv[4]);
    minmax_open_buf(&map, argv[2], 8, nb_slices);

    ASSERT_ERRNO(chdir(argv[1]));
    OS_THREAD_START(http_thread, http_loop, NULL);

    return la_loop();
}
