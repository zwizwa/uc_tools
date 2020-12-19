#ifndef MOD_CONSOLE_C
#define MOD_CONSOLE_C

/* This uses the uc_tools linker support to create the command
   dictionary in a distributed way.

   Each module can register commands, and we don't need to reference
   the list of commands explicitly. */

#include "command.h"

#include "pbuf.h"

/* Support for command.h style commands. */
struct pbuf console_pbuf; uint8_t console_pbuf_buf[128];

/* Basic stack implementation and generic Forth commands. */
#include "mod_command_stack.c"
#include "mod_forth_commands.c"

void console_handle(const char *cmd) {
    command_handle(cmd);
}

void console_write(const uint8_t *buf, uint32_t len) {
    struct pbuf *p = &console_pbuf;
    for(uint32_t i=0; i<len; i++) {
        uint8_t c = buf[i];
        switch(c) {
        case '\t':
        case '\n':
        case '\r':
        case ' ':
            /* Whitespace.  Skip if we don't have anything yet.
             * Otherwise treat as delimiter. */
            if (p->count > 0) {
                pbuf_zero_terminate(p);
                console_handle((const char*)p->buf);
                pbuf_clear(p);
            }
            break;
        case 127:
            /* Rudimentary editing: just backspace. */
            if (p->count) p->count--;
            break;
        default:
            if ((c >= 32) && (c < 127)) {
                pbuf_put(p, c);
            }
            else {
                /* Just ignore special characters. */
            }
            break;
        }
    }
}
void console_write_echo(const uint8_t *buf, uint32_t len) {
    while(len) {
        uint8_t c = *buf++; len--;
        /* Who came up with this shit really? */
        if (c == 127) {
            char backspace[] = {8,' ',8,0};
            info_puts(backspace);
        }
        else {
            info_putchar(c);
            if (c == '\r') { info_putchar('\n'); }
        }
        console_write(&c, 1);
    }
}

/* Support for GDB monitor command. */
const char console_newline[] = "\n";
const char *console_monitor(const char *command) {
    console_write((const uint8_t*)command, strlen(command));
    console_write((const uint8_t*)console_newline,1);
    /* Replies current ly don't seem to work, but we're not using them
       anyway as all output goes to the console log.  Send a non-null
       dummy value.  NULL means command not supported, which we don't
       know here anyway. */
    return console_newline;
}

const struct gdbstub_io standalone_io = {
    .write = console_write_echo,
    .read  = info_read_crlf,
};

/* Allow smart protocol switcher in the case where the main protocol
   is SLIP.  This needs to be robust against operating systems trying
   to send random stuff to the serial port when plugged in.  In exo,
   the first thing that is sent is an empty SLIP packet. */
int console_switch_protocol(const uint8_t *buf, uint32_t size) {
    int is_slip = (size > 1) && (buf[0] == SLIP_END);
    if (!is_slip) {
        *_service.io = (struct gdbstub_io *)(&standalone_io);
        (*_service.io)->write(buf, size);
        return 1;
    }
    return 0;
}

void console_init(void) {
    PBUF_INIT(console_pbuf);
}

#endif
