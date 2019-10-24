/* Plugin example.  See code at bottom. */

#include "base.h"

/* Revisiting Forth

   This time, think about the tradeoffs.  There are a lot of different
   ways to do this and they all have advantages and disadvantages.
   What are the requirements and relaxations?

   - Speed is not terribly important

   - Mostly, this needs to be simple and able to share the Forth
     library with other implementations.

   - Tail call optimization is a plus

   - Primitives should be C functions to avoid the "dual language
     problem".  This precludes a direct or indirect threaded
     interpreter and leaves a loop threaded or subroutine threaded
     interpreter.

   - A subroutine threaded interpreter requires the R stack to be the
     C return stack, which will not make it possible to write all
     primitives in C.  Also, if R is separate, multitasking becomes
     simple to implement, so loop threading is what's left.

   - Get an interactive console working as soon as possible.

   - Use a "standard" Forth outer interpreter and syntax.  eForth
     comes to mind.  I don't really want to bootstrap an outer
     interpreter.  (EDIT: This might not be so important.  It is a lot
     of work to bootstrap.)

   - Use circular stacks

*/

union word;
typedef void (*code_fn)(union word *);
union word {
    int i;
    uint32_t u32;
    uint32_t *u32p;
    code_fn code;
    union word *pw;
    const union word *cpw;
};
typedef union word w;

#include "cbuf.h"


/* Some design constraints for the inner interpreter.

   - We do not have a "next" routine in code words.  All code words
     end in return so they are easier to mix with C code.

   - An instruction or execution token (XT) is a pointer into the
     dictionary, pointing to the code word entry that interprets the
     code list (e.g. ENTER).

   - It is simpler to keep the type of ip to be a pointer to union
     word, and do the double dereference inside the interpreter loop.

   - Due to pointer alignment, we have room to encode special
     interpreter opcodes, e.g. to break out of the main loop.

   - It is very tempting to use the THUMB bit to distinguish machine
     code from execuion tokens, so that is exectly what we're going to
     do.  This avoids the need for wrapping each primitive in a
     dictionary entry.

   - Taking this further, all token lists are going to be 32-bit
     aligned, so there are actually 2 extra tag codes to use.
*/

w *ip;

/* Special interpreter opcodes */
#define IOPC(x) (2 | ((x)<<2))
#define YIELD IOPC(0)
// TODO: blocking read


void interpreter(void) {
    //int max = 10;

    for(;;) {
        w xt = *ip;
        infof("ip:%08x xt:%08x\n", ip, xt);
        ip++;

        switch (xt.u32 & 3) {
        case 0: {
            (xt.pw)->code(xt.pw+1);
            break;
        }
        case 1: // Thumb tag
        case 3: // Possibly free tag due to alignment?
            xt.code(0);
            break;
        case 2: // Interpreter control.
            if (YIELD == xt.u32) return;
            break;
        }
        //if (!(max--)) return;
    }
}

void run(w xt) {
    /* This needs a wrapper word. */
    w wrap[] = {xt,(w)YIELD};
    ip = wrap;
    interpreter();
}


/* Output needs to be buffered for USB polling.  Input is buffered as
 * well because the Forth interpreter uses a pull interface input, not
 * a push interface that we could call when data comes in. */
uint8_t     forth_out_buf[64];
struct cbuf forth_out;

uint8_t     forth_in_buf[64];
struct cbuf forth_in;


#define DS_LOGSIZE 5
#define DS_SIZE (1 << DS_LOGSIZE)
#define DS_MASK (DS_SIZE-1)

#define RS_LOGSIZE 5
#define RS_SIZE (1 << RS_LOGSIZE)
#define RS_MASK (RS_SIZE-1)

w ds[DS_SIZE]; uint32_t di;
w rs[DS_SIZE]; uint32_t ri;

#define TOP ds[di&DS_MASK]
#define SND ds[(di-1)&DS_MASK]

#define TOPR rs[ri&RS_MASK]

static void push(w a) { di++; TOP = a; }
static w pop(void) { w rv = TOP; di--; return rv; }

static void pushr(w a) { ri++; TOPR= a; }
static w popr(void) { w rv = TOPR; ri--; return rv; }

// static w index(uint32_t i) { return ds[(di + i) & DS_MASK]; }

static void dup(w* _) { di++; TOP = SND; }
static void add(w* _) { SND.u32 += TOP.u32; pop(); }
static void print(w* _) { infof("%08x\n", pop().u32); }


// ?RX ( -- c T | F )
// Return input character and true, or a false if no input.
static void rx(w* _) {
    if (cbuf_empty(&forth_in)) {
        push((w)(uint32_t)0);
    }
    else {
        push((w)(uint32_t)cbuf_get(&forth_in));
        push((w)(uint32_t)-1);
    }
}
// TX! ( c -- )
static void tx(w* _) {
    cbuf_put(&forth_in, pop().u32);
}

// Inner interpreter
static void LIT(w* _) {
    push(*ip++);
}
static void push_ip(void) {
    w _ip_old = {.pw = ip};
    pushr(_ip_old);
}
static void ENTER(w* list) {
    push_ip();
    ip = list;
}
static void EXIT(w* _) {
    ip = popr().pw;
}
static void EXECUTE(w* _) {
    push_ip();
    ip = pop().pw;
}

struct dict {
    const char *name;
    code_fn code;
};



struct dict dict[] = {
    // eForth primitives
    // System interface
    {"?rx", rx},
    {"tx!", tx},
    // Inner interpreter
    {"LIT", LIT},
    {"ENTER", ENTER},
    {"EXIT",EXIT},
    {"EXECUTE", EXECUTE},
#if 0
    {"?branch", TODO},
    {"branch", TODO},
    // Memory access
    {"!",TODO},
    {"@",TODO},
    {"C!",TODO},
    {"C@",TODO},
    // Return stack
    {"RP@",TODO},
    {"RP!",TODO},
    {"R>",TODO},
    {"R@",TODO},
    {">R",TODO},
    // Data stack
    {"SP@",TODO},
    {"SP!",TODO},
#endif
    {"DROP",(code_fn)pop},
    {"DUP",dup},
#if 0
    {"SWAP",TODO},
    {"OVER",TODO},
    //Logic
    {"0<",TODO},
    {"AND",TODO},
    {"OR",TODO},
    {"XOR",TODO},
    // Arithmetic
    {"UM+",TODO},
#endif
    {}
};

/* If an on-target outer interpreter is necessary, the high level word
   dictionary is best bootstrapped from another eForth image or from
   an outer interpreter written in another language.  Writing
   primitives in C can be done like this ... */


const w lit1[] = { (w)ENTER, (w)LIT, (w)1, (w)EXIT };
const w test[] = { (w)ENTER, (w)lit1, (w)lit1, (w)add, (w)print, (w)EXIT };

/* ... but gets tedious when conditional jumps are involved.

   That said I've never liked implementing the outer interpreter in
   Forth.  I only interact with these things when I have another
   computer available that can run some meta code.  So for now, see
   where this is heading. */


/* TAG_PLUGIO stream will be routed here. */
uint32_t forth_read(uint8_t *buf, uint32_t size) {
    return cbuf_read(&forth_out, buf, size);
}
void forth_write(const uint8_t *buf, uint32_t len) {
    cbuf_write(&forth_out, buf, len);
}
void forth_start(void) {
    CBUF_INIT(forth_in);
    CBUF_INIT(forth_out);
    const uint8_t hello[] = "hello forth\r\n";
    cbuf_write(&forth_out, hello, sizeof(hello)-1);

    infof("(pre) di = %d\n", di);
    run((w)test);
    infof("(post) di = %d\n", di);

}


#include "plugin_api.h"
#include "gdbstub_api.h"


/* Loader calls this after reflashing. */
static void plugin_start(void) {
    plugin_init_memory();
    forth_start();
}
/* Header at start of .bin file */
struct plugin_service plugin PLUGIN_HEADER_SECTION = {
    .version = PLUGIN_API_VERSION,
    .io = { .read = forth_read, .write = forth_write },
    .start = plugin_start
};




