#ifndef FORTH_H
#define FORTH_H

#include "gdbstub_api.h" 

/* Revisiting Forth

   - A "protocol oriented" language (e.g. design protocol to keep
     parser simple).

   - Avoid the dual-language problem by using the same ABI in C and
     Forth, at least for 0-op functions.  This should avoid the dual
     language issue.  I.e. it will be trival to convert (textual)
     Forth to (textual) C.

   - Start out interactively.  E.g. before doing anything, a Forth is
     running on the console.

   - Use a "standard" Forth approach.  Maybe start from eforth?


   Optional:

   - A simple way to allow for state machines to be implemented as
     processes.  This requires an actual task switcher.

*/

/* FIXME: Been thinking about this, and it seems to be quite an
 * endeavor.  The irony is that using a subroutine threaded Forth, it
 * is almost impossible to write primitives in C because of return
 * stack access.  Also, task switching is not possible without C stack
 * switching.  So maybe it is just not a good idea to make the Forth C
 * compatible? */

union word {
    uint32_t u32;
    uint32_t *u32p;
};
typedef union word w;


void forth_start(void);
uint32_t forth_read(uint8_t *buf, uint32_t size);
void forth_write(const uint8_t *buf, uint32_t len);

struct forth {
    struct gdbstub_io io;
};

#define FORTH_INIT { .io = {.read = forth_read, .write = forth_write }}

#endif
