#ifndef SYMBOL_H
#define SYMBOL_H

#include "log.h"
#include "tools.h"
#include <string.h>

/* Similar to command.h

   The idea here is to have a dictionary that can map symbol strings
   to identifiers. This requires linker script support to create the
   global symbol array. */

/* Contains pointers to init function for static symbols. */
#define SYMBOL_SECTION      __attribute__ ((section (".symbol")))

/* The symbol section is only an index, i.e. it collects only
   pointers to C strings. */
extern const char *_symbol_start;
extern const char *_symbol_endx;

static inline uintptr_t symbol_index(const char* const* sym) {
    return sym - &_symbol_start;
}
static inline const char* const* symbol_ref(uintptr_t index) {
    return &_symbol_start + index;
}
static inline uintptr_t symbol_index_size(void) {
    return &_symbol_endx - &_symbol_start;
}

#define FOR_SYMBOL(c) \
    FOR_START_ENDX(&_symbol_start, &_symbol_endx, c)

#define DEF_SYMBOL(name) \
    const char * symbol_##name SYMBOL_SECTION = #name

#define SYMBOL_INDEX(name) \
    symbol_index(&symbol_##name)


#endif
