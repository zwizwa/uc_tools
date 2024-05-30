#ifndef MOD_TELNET
#define MOD_TELNET

#include "macros.h"

#include <stdint.h>
#include <string.h>

/* Minimal subset of Telnet.  Originally implemented to talk to target
   over RTT TCP socket provided by OpenOCD.  I don't have time to read
   and understand all docs, so this is implemented using trial and
   error.

   https://en.wikipedia.org/wiki/Telnet
   https://www.rfc-editor.org/rfc/rfc854
   https://www.iana.org/assignments/telnet-options/telnet-options.xhtml

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

    TELNET_COMMAND_IAC,
    TELNET_COMMAND_WILL,
    TELNET_OPTION_ECHO,

    TELNET_COMMAND_IAC,
    TELNET_COMMAND_WILL,
    TELNET_OPTION_SUPPRESS_GO_AHEAD,

    TELNET_COMMAND_IAC,
    TELNET_COMMAND_WONT,
    TELNET_OPTION_LINEMODE

};

struct telnet;
typedef void (*telnet_write_output_fn)(struct telnet *, const uint8_t *, uintptr_t);
struct telnet {
    void *next;
    /* Set it up so that we push characters to output.  If it needs to
       be buffered then this can push into buffer implemented by
       caller. */
    telnet_write_output_fn write_output;
    void (*interrupt)(struct telnet *);
    uint8_t cmd, opt;
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

static inline void telnet_tick(struct telnet *s, uint8_t telnet_tick_input) {
    if (s->next) goto *(s->next);
  next:
    TELNET_NEXT(s, s->cmd);
    /* For now, just ignore all commands.  Just parse them so they are
       not interpreted as data. */
    switch(s->cmd) {
    case TELNET_COMMAND_IAC:
        /* No action needed. */
        goto next;
    case TELNET_COMMAND_DO:
    case TELNET_COMMAND_DONT:
    case TELNET_COMMAND_WILL:
    case TELNET_COMMAND_WONT:
        /* No action needed except for reading the option byte. */
        TELNET_NEXT(s, s->opt);
        LOG("ign %02x %02x\n", s->cmd, s->opt);
        goto next;
    case '\r':
        LOG("\n");
        goto next;
    case 0:
        /* Why is it sending 0 on CR?  Add a mode to debug the control
           characters. */
        goto next;
        /* When linemode is off, CTRL-C is just a character. */
    case TELNET_COMMAND_INTERRUPT:
    case 3:
        if (s->interrupt) { s->interrupt(s); }
        goto next;
    default:
        /* When linemode is off, CTRL-C etc are sent as bytes. */
        if (s->cmd < 32) {
            LOG("<%d>", s->cmd);
        }
        else {
            /* Everything else is data */
            // LOG("%02x\n", byte);
            uint8_t new_byte = s->cmd;
            // s->write_output(s, &new_byte, 1); // echo, directly
            LOG("%c", new_byte); // echo, indirectly via info log
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

static inline void telnet_init(struct telnet *s, telnet_write_output_fn write_output) {
    memset(s, 0, sizeof(*s));
    s->write_output = write_output;
    write_output(s, telnet_init_bytes, sizeof(telnet_init_bytes));
    telnet_tick(s, 0 /* dummy, run it up to first NEXT */);
}




#endif
