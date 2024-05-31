#ifndef MOD_TELNET
#define MOD_TELNET

#include "macros.h"

#include <stdint.h>
#include <string.h>

/* Ad-hoc minimal subset of Telnet.  Originally implemented to talk to
   target over RTT TCP socket provided by OpenOCD.  I don't have time
   to read and understand all docs, so this is implemented using trial
   and error, just to get a command console up.

   https://en.wikipedia.org/wiki/Telnet
   https://www.rfc-editor.org/rfc/rfc854
   https://www.iana.org/assignments/telnet-options/telnet-options.xhtml
   https://en.wikipedia.org/wiki/ANSI_escape_code
   https://en.wikipedia.org/wiki/C0_and_C1_control_codes
*/

/* Notes:

   - In order to sync with other logging mechanism there should be a
     flush operation that passes log data to write_outpit()

   - TODO: Split off the ANSI line editor part, because that would
     work on serial port as well.

*/

#define TELNET_COMMAND_INTERRUPT 244 // F4
#define TELNET_COMMAND_WILL      251 // FB
#define TELNET_COMMAND_WILL      251 // FB
#define TELNET_COMMAND_WONT      252 // FC
#define TELNET_COMMAND_DO        253 // FD
#define TELNET_COMMAND_DONT      254 // FE
#define TELNET_COMMAND_IAC       255 // FF


#define TELNET_OPTION_ECHO                 1 // 01
#define TELNET_OPTION_SUPPRESS_GO_AHEAD    3 // 03
#define TELNET_OPTION_LINEMODE            34
#define TELNET_OPTION_SUPPRESS_LOCAL_ECHO 45 // 2D


/* We support only character mode. */
const uint8_t telnet_init_bytes[] = {
    // https://stackoverflow.com/questions/273261/force-telnet-client-into-character-mode

    // Telnet sequence to switch to character mode.

    TELNET_COMMAND_IAC,
    TELNET_COMMAND_WILL,
    TELNET_OPTION_ECHO,

    TELNET_COMMAND_IAC,
    TELNET_COMMAND_WILL,
    TELNET_OPTION_SUPPRESS_GO_AHEAD,

    TELNET_COMMAND_IAC,
    TELNET_COMMAND_WONT,
    TELNET_OPTION_LINEMODE,

    // Escape code to clear screen
    27, '[', 'H',  27, '[', 'J',

};

#ifndef TELNET_LOG
#define TELNET_LOG LOG
#endif

#ifndef TELNET_LINE_BUF_SIZE
#define TELNET_LINE_BUF_SIZE 128
#endif

#ifndef TELNET_ESCAPE_BUF_SIZE
#define TELNET_ESCAPE_BUF_SIZE 13 // How many are needed here?
#endif

#define TELNET_EVENT_LINE       0x100
#define TELNET_EVENT_ESCAPE     0x200
#define TELNET_EVENT_INTERRUPT  0x300
#define TELNET_EVENT_FLUSH      0x400
#define TELNET_EVENT_CONTROL    0x500


struct telnet;
typedef void (*telnet_write_output_fn)(struct telnet *, const uint8_t *, uintptr_t);
typedef void (*telnet_event_fn)(struct telnet *, uintptr_t);
struct telnet {
    void *next;
    /* Set it up so that we push characters to output.  If it needs to
       be buffered then this can push into buffer implemented by
       caller. */
    telnet_write_output_fn write_output;
    telnet_event_fn event;
    uint8_t cmd, opt, nb_esc, nb_char;
    uint8_t esc[TELNET_ESCAPE_BUF_SIZE];
    uint8_t line[TELNET_LINE_BUF_SIZE];
};

/* Machine is written in push style, waiting for next byte.
   Each occurrence of NEXT is a suspend point. */
#define TELNET_NEXT_(s,var,label)                       \
    do {						\
	s->next = &&label;				\
	return;                                         \
      label: 						\
        (var) = telnet_tick_input;                      \
    } while(0)
#define TELNET_NEXT(s,var)                      \
    TELNET_NEXT_(s,var,GENSYM(label_))

static inline void telnet_tick(struct telnet *s, int32_t telnet_tick_input) {
    if (telnet_tick_input >=0 ) { 
        TELNET_LOG("%02x (%d)\n", telnet_tick_input, telnet_tick_input);
    }
    // TELNET_LOG("[%02x]", telnet_tick_input);
    if (s->next) goto *(s->next);
  next:
    TELNET_NEXT(s, s->cmd);
    switch(s->cmd) {
    case TELNET_COMMAND_IAC:
        /* For now, just parse and ignore telnet commands. */
        TELNET_NEXT(s, s->cmd);
        switch(s->cmd) {
        case TELNET_COMMAND_INTERRUPT:
            TELNET_LOG("<IAC,INTERRUPT>");
            goto interrupt;
        case TELNET_COMMAND_DO:
        case TELNET_COMMAND_DONT:
        case TELNET_COMMAND_WILL:
        case TELNET_COMMAND_WONT:
            /* No action needed except for reading the option byte. */
            TELNET_NEXT(s, s->opt);
            TELNET_LOG("<IAC,CMD:%d,OPT:%d>\n", s->cmd, s->opt);
            goto next;
        default:
            TELNET_LOG("<IAC,%02x>", s->cmd);
            goto next;
        }

        /* All te rest is ANSI terminal codes */

    case '\r': {
        const uint8_t buf[] = {'\r','\n'};
        s->event(s, TELNET_EVENT_FLUSH);
        s->write_output(s, buf, sizeof(buf));
        s->event(s, TELNET_EVENT_LINE);
        s->nb_char = 0;
        goto next;
        }
    case 3:
        interrupt:
        s->event(s, TELNET_EVENT_FLUSH);
        s->event(s, TELNET_EVENT_INTERRUPT);
        goto next;
    case 127:
        if (s->nb_char > 0) {
            const uint8_t buf[] = {'\b',' ','\b'};
            s->write_output(s, buf, sizeof(buf));
            s->nb_char--;
        }
        goto next;
    case 27:
        // FIXME: Probably not complete
        /* Reuse cmd,opt for ESC codes */
        TELNET_NEXT(s, s->cmd);
        s->nb_esc = 0;
        s->esc[s->nb_esc++] = s->cmd;
        if (s->cmd == '[') {

            /* Control Sequence Introducer, or CSI, has the following
               structure:

               - any number (including none) of "parameter bytes"
               in the range 0x30–0x3F ASCII 0–9:;<=>?

               - any number of "intermediate bytes" in the range
               0x20–0x2F !"#$%&'()*+,-./ and space

               - a single "final byte" in the range 0x40–0x7E
               @A–Z[\]^_`a–z{|}~ */

            for(;;) {
                TELNET_NEXT(s, s->opt);
                if (s->nb_esc < sizeof(s->esc)) {
                    s->esc[s->nb_esc++] = s->opt;
                }
                if ((s->opt >= 0x40) && (s->opt <=0x7E)) break;
            }
        }
        else if ((s->cmd >= 0x40 && (s->cmd < 0x5F))) {
            /* Fe ESC Code */
            TELNET_NEXT(s, s->opt);
            s->esc[s->nb_esc++] = s->opt;
        }
        else {
            TELNET_LOG("<FIXME:ESC 0x%02x>", s->cmd);
        }
        s->event(s, TELNET_EVENT_ESCAPE);
        s->nb_esc = 0;
        goto next;

    default:
        /* When linemode is off, CTRL-C etc are sent as bytes. */
        if (s->cmd < 32) {
            // FIXME: Hitting enter seems to also send a 00 byte.
            s->event(s, TELNET_EVENT_CONTROL + s->cmd);
        }
        else {
            s->event(s, TELNET_EVENT_FLUSH);

            /* Everything else is data */
            uint8_t new_byte = s->cmd;
            if (s->nb_char < sizeof(s->line)) {
                s->line[s->nb_char++] = new_byte;
            }
            s->write_output(s, &new_byte, 1); // echo, directly
        }
        goto next;
    }
}

static inline void telnet_write_input(struct telnet *s,
                                      const uint8_t *bytes, uintptr_t len) {
    for (uintptr_t i=0; i<len; i++) {
        telnet_tick(s, bytes[i]);
    }
}

static inline void telnet_init(struct telnet *s,
                               telnet_write_output_fn write_output,
                               telnet_event_fn event) {
    memset(s, 0, sizeof(*s));
    s->write_output = write_output;
    s->event = event;
    write_output(s, telnet_init_bytes, sizeof(telnet_init_bytes));
    telnet_tick(s, -1 /* dummy, run it up to first NEXT */);
}




#endif
