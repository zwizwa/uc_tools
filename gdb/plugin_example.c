/* Plugin example: a Forth. */

#include "base.h"
#include "plugin_api.h"

#include "gdbstub_api.h"

/* Revisiting Forth

   This time, think about the tradeoffs.  There are a lot of different
   ways to do this and they all have advantages and disadvantages.
   What are the requirements and relaxations?

   - Speed is not terribly important

   - Mostly, this needs to be simple

   - Tail call optimization is necessary

   - Primitives should be C functions to avoid the "dual langauge
     problem".  This precludes a directly or indirectly threaded
     interpreter and leaves a loop threaded or subroutine threaded
     interpreter.

   - A subroutine threaded interpreter requires the R stack to be the
     C return stack, which will not make it possible to write all
     primitives in C.  If R is separate, multitasking becomes simple
     to implement.

   - Get an interactive console working as soon as possible.

   - Use a "standard" Forth outer interpreter and syntax.  eForth
     comes to mind.

   - Use circular stacks

*/

union word {
    uint32_t u32;
    uint32_t *u32p;
};
typedef union word w;

#include "cbuf.h"

/* Output needs to be buffered for USB polling. */
uint8_t     forth_out_buf[64];
struct cbuf forth_out;


#define DS_LOGSIZE 5
#define DS_SIZE (1 << DS_LOGSIZE)
#define DS_MASK (DS_SIZE-1)

#define RS_LOGSIZE 5
#define RS_SIZE (1 << RS_LOGSIZE)
#define RS_MASK (RS_SIZE-1)

w ds[DS_SIZE]; uint32_t di;
w rs[DS_SIZE]; uint32_t ri;

#define TOP ds[di&DS_MASK]
#define SND ds[(di+1)&DS_MASK]

//static void push(w word) { di++; TOP = word; }
static w pop(void) { w rv = TOP; di--; return rv; }

// static w index(uint32_t i) { return ds[(di + i) & DS_MASK]; }

static void dup(void) { di++; TOP.u32 = SND.u32; }
static void add(void) { SND.u32 += TOP.u32; pop(); }

typedef void (*prim_fn)(void);

struct dict {
    const char *name;
    prim_fn prim;
};
struct dict dict[] = {
    {"drop", (prim_fn)pop},
    {"dup", dup},
    {"+", add},
    {}
};


/* TAG_PLUGIO stream will be routed here. */
uint32_t forth_read(uint8_t *buf, uint32_t size) {
    return cbuf_read(&forth_out, buf, size);
}
void forth_put(uint8_t byte) {
    cbuf_write(&forth_out, &byte, 1);
}
void forth_write(const uint8_t *buf, uint32_t len) {
    while(len--) forth_put(*buf++);
}
void forth_start(void) {
    CBUF_INIT(forth_out);
    const uint8_t hello[] = "hello forth\r\n";
    cbuf_write(&forth_out, hello, sizeof(hello)-1);
}




/* Loader calls this after reflashing. */

extern uint8_t _stext;
extern uint8_t _etext;

#include "crc.h"



static void plugin_start(void) {
    plugin_init_memory();
    uint32_t cs = crc32b(&_stext, &_etext - &_stext);
    infof("plugin_start S:%08x L:%d CRC:%08x\n", &_stext, &_etext - &_stext, cs);
    forth_start();
}

/* Header at start of .bin file */
struct plugin_service plugin PLUGIN_HEADER_SECTION = {
    .version = PLUGIN_API_VERSION,
    .io = { .read = forth_read, .write = forth_write },
    .start = plugin_start
};



#if 0
// Defined in doodle.c
void set_pin(int pin, int val);
KEEP void set_pin_delegate(int pin, int val) {
    set_pin(pin, val);
}
#endif



