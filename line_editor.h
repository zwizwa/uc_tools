#ifndef LINE_EDITOR_H
#define LINE_EDITOR_H

#include "pbuf.h"
#include <stdint.h>

/* This connects the (raw) in/out of a serial port to a callback that
   interprets a line of commands. */
typedef void (*line_editor_echo_fn)(void *, uint8_t byte);
typedef void (*line_editor_line_fn)(void *, const uint8_t *buf, uint32_t len);

#ifndef LINE_EDITOR_ESCAPE_BUF_SIZE
#define LINE_EDITOR_ESCAPE_BUF_SIZE 12 // How many are needed here?
#endif

struct line_editor {
    void *next;
    line_editor_echo_fn echo;
    line_editor_line_fn line;
    void *ctx;
    struct pbuf *p;

    uint8_t byte, cmd, opt, nb_esc;
    uint8_t esc[LINE_EDITOR_ESCAPE_BUF_SIZE];
};

/* Push a terminal input byte into the editor. */
static inline void line_editor_puts(struct line_editor *s, const char *str) {
    while(*str) { s->echo(s->ctx, *str++); }
}


/* Machine is written in push style, waiting for next byte.
   Each occurrence of NEXT is a suspend point.
   See mod_telnet.c  (which could probably reuse some of this machine. ) */
#define LINE_EDITOR_NEXT_(s,var,label)                  \
    do {						\
	s->next = &&label;				\
	return;                                         \
      label: 						\
        (var) = line_editor_input;                      \
    } while(0)
#define LINE_EDITOR_NEXT(s,var)                 \
    LINE_EDITOR_NEXT_(s,var,GENSYM(label_))


static inline void line_editor_push(struct line_editor *s,
                                    uint8_t line_editor_input) {
    // LOG("[%02x]", line_editor_input);
    // DEL [1b][5b][33][7e]

    if (s->next) goto *s->next;
  next:
    LINE_EDITOR_NEXT(s, s->byte);
    switch(s->byte) {
    case 27:
        goto esc;
    case '\r':
        pbuf_put(s->p, 0);
        LOG("\nline: %s\n", s->p->buf);
        goto next;
    case 8: // Backspace
        line_editor_puts(s, "\x08\x1B\x5B\x50");
        goto next;
    default:
        pbuf_put(s->p, s->byte);
        s->echo(s->ctx, s->byte);
        goto next;
    }

  esc:
    LINE_EDITOR_NEXT(s, s->cmd);
    if (s->cmd == 27) {
        /* Ignore double escape. */
        goto esc;
    }
    else if (s->cmd == '[') {
        /* Control sequence introducer.

           - any number (including none) of "parameter bytes"
             in the range 0x30–0x3F ASCII 0–9:;<=>?

           - any number of "intermediate bytes"
             in the range 0x20–0x2F !"#$%&'()*+,-./ and space

           - a single "final byte" in the range 0x40–0x7E
             @A–Z[\]^_`a–z{|}~ */

        for(;;) {
            LINE_EDITOR_NEXT(s, s->opt);
            if (s->nb_esc < sizeof(s->esc)) {
                s->esc[s->nb_esc++] = s->opt;
            }
            if ((s->opt >= 0x40) && (s->opt <=0x7E)) break;
        }

    }
    else if ((s->cmd >= 0x40 && (s->cmd < 0x5F))) {
        /* Fe ESC Code */
        LINE_EDITOR_NEXT(s, s->opt);
        s->esc[s->nb_esc++] = s->opt;
    }
    else {
        // FIXME
    }
    s->nb_esc = 0;
    goto next;
}

static inline void line_editor_init(
    struct line_editor *s,
    struct pbuf *p,
    line_editor_echo_fn echo,
    line_editor_line_fn line,
    void *ctx)
{
    memset(s, 0, sizeof(*s));
    s->p = p;
    s->echo = echo;
    s->line = line;
    s->ctx = ctx;
    /* Dummy tick, run task up to NEXT, waiting for first byte. */
    line_editor_push(s, -1);
}

#endif
