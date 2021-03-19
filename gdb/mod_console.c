#ifndef MOD_CONSOLE
#define MOD_CONSOLE

/* This uses the uc_tools linker support to create the command
   dictionary in a distributed way.

   Each module can register commands, and we don't need to reference
   the list of commands explicitly. */

#include "command.h"
#include "pbuf.h"
#include "instance.h"

/* Support for command.h style commands. */
struct pbuf console_in; uint8_t console_in_buf[64];

/* Temporary buffer for console commands. */
#ifndef TMP_BUF_SIZE
#define TMP_BUF_SIZE 64
#endif
struct pbuf console_tmp; uint8_t console_tmp_buf[TMP_BUF_SIZE];

uintptr_t console_base = 10;
DEF_COMMAND(hex)     { console_base = 16; }
DEF_COMMAND(decimal) { console_base = 10; }

intptr_t console_echo = 1;
DEF_COMMAND(echo)    { console_echo = command_stack_pop(); }


void console_handle(const char *cmd) {
    command_handle_base(cmd, console_base);
}
void console_write(const uint8_t *buf, uint32_t len) {
    struct pbuf *p = &console_in;
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
        if (console_echo) {
            /* Who came up with this shit really? */
            if (c == 127) {
                char backspace[] = {8,' ',8,0};
                info_puts(backspace);
            }
            else {
                info_putchar(c);
                if (c == '\r') { info_putchar('\n'); }
            }
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


/* Circular argument stack. */
#ifndef COMMAND_STACK_LOGSIZE
#define COMMAND_STACK_LOGSIZE 3
#endif
#define COMMAND_STACK_MASK ((1<<COMMAND_STACK_LOGSIZE)-1)
uintptr_t command_stack[COMMAND_STACK_MASK+1];
uintptr_t command_stack_top;
void command_stack_push(uintptr_t word) {
    command_stack[(--command_stack_top) & COMMAND_STACK_MASK] = word;
}
uintptr_t command_stack_pop(void) {
    return command_stack[(command_stack_top++) & COMMAND_STACK_MASK];
}

/* These have non-C names. */
void forth_add(void) {
    command_stack_push(command_stack_pop() + command_stack_pop());
}
void forth_sub(void) {
    uint32_t v = command_stack_pop(); command_stack_push(command_stack_pop() - v);
}
void forth_print(void) {
    infof("%x\n", command_stack_pop());
}
void forth_fetch(void) {
    uint32_t* addr = (void*)command_stack_pop();
    command_stack_push(*addr);
}
void forth_store(void) {
    uint32_t* addr = (void*)command_stack_pop();
    uint32_t val = command_stack_pop();
    *addr = val;
}
COMMAND_REGISTER_NAMED("+", forth_add);
COMMAND_REGISTER_NAMED("-", forth_sub);
COMMAND_REGISTER_NAMED("p", forth_print);
COMMAND_REGISTER_NAMED("@", forth_fetch);
COMMAND_REGISTER_NAMED("!", forth_store);


/* Don't use the name 'words' here.  Pick something that people expect. */
DEF_COMMAND(commands) {
    infof("commands: ");
    FOR_COMMAND(c) { infof("%s ",(*c)->name); };
    infof("\n");
}
DEF_COMMAND(clear) {
    pbuf_clear(&console_tmp);
}
void tmp_buf_write_uint(uint32_t nb) {
    uint8_t buf[nb];
    write_be(buf, command_stack_pop(), nb);
    pbuf_write(&console_tmp, buf, nb);
}
DEF_COMMAND(u8)   { tmp_buf_write_uint(1); }
DEF_COMMAND(u16)  { tmp_buf_write_uint(2); }
DEF_COMMAND(u32)  { tmp_buf_write_uint(4); }
DEF_COMMAND(ptmp) {
    struct pbuf *p = &console_tmp;
    infof("%d:", p->count);
    info_hex_u8(p->buf, p->count);
    infof("\n");
}

//DEF_COMMAND_NAMED("buf", tmp_buf_span) {
//    command_arg_push(tmp_buf.count);
//    command_arg_push((uintptr_t)tmp_buf.buf);
//}

instance_status_t console_init(instance_init_t *ctx) {
    PBUF_INIT(console_in);
    PBUF_INIT(console_tmp);
    return 0;
}
DEF_INSTANCE(console);

#endif
