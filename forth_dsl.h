#ifndef FORTH_DSL_H
#define FORTH_DSL_H

/* FIXME: Properly split this into 3 levels:
   1. C API
   2. Forth threaded code API
   3. Forth inner interpreter implementation
*/

/* See commments in mod_forth_dsl.c */

/* API */
#include <stdint.h>
#include "cbuf.h"
struct forth_dsl_state;
void forth_dsl_init(struct forth_dsl_state *s);
void forth_dsl_write(struct forth_dsl_state *s, const uint8_t *buf, uint32_t len);


typedef uint16_t CELL;

/* Expose the threaded code bits.  This is all that is needed to
   generate threaded code, i.e. all except the actual interpreter of
   which there can be a couple of variants. */

#ifndef FORTH_DSL_RAM_SIZE
#define FORTH_DSL_RAM_SIZE 64
#endif

#ifndef FORTH_DSL_IOBUF_SIZE
#define FORTH_DSL_IOBUF_SIZE 64
#endif

struct forth_dsl_state {
    uint16_t *ds,*rs;
    uint16_t ip, here;
    const uint16_t *rom;
    struct cbuf out;
    uint8_t out_buf[FORTH_DSL_IOBUF_SIZE];
    /* Keep this at the end so it is easily extended. */
    uint16_t ram[FORTH_DSL_RAM_SIZE];
};

/* For now keep it simple and just split half/half. */
#define FORTH_DSL_ROM_SIZE 0x8000

/* 64k words of virtual memory. */
static __attribute__((__const__)) CELL *
forth_dsl_mem(struct forth_dsl_state *s, CELL addr)  {
    if (addr < FORTH_DSL_ROM_SIZE) return (CELL*)&s->rom[addr];
    return &s->ram[addr - FORTH_DSL_ROM_SIZE];
}


#endif
