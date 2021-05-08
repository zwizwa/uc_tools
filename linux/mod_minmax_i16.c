#ifndef MOD_MINMAX_I16X1
#define MOD_MINMAX_I16X1

// This implements the more general instantiated minmax api
#define MOD_MINMAX

#include "mmap_file.h"

#include <stdint.h>

#define NS(name) minmax##name

/* We provide the base type.  (cfr. module argument) Note that the
   type doesn't need to be scalar.  It can be a vector of channels.
   See mod_minmax_logic.c */

typedef int16_t NS(_t);
static inline NS(_t) NS(_max)(NS(_t) a, NS(_t) b) { return a > b ? a : b; }
static inline NS(_t) NS(_min)(NS(_t) a, NS(_t) b) { return a < b ? a : b; }

/* Functionality is parameterized by base type (cfr. functor, module function) */
#include "ns_minmax.h"
#undef NS

#include "macros.h"


/* FIXME: This needs to handle all mouse events:
   - Wheel to zoom in/out
   - Drag to move the window
   - Marker insert
   - Coordinates are original file coordinates.
*/



#endif
