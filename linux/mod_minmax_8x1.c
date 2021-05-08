#ifndef MOD_MINMAX_1X8
#define MOD_MINMAX_1X8

// This implements the more general instantiated minmax api
#define MOD_MINMAX

#include "mmap_file.h"

#include <stdint.h>

#define NS(name) minmax##name

/* We provide the base type.  (cfr. module argument).

   Base type is an array of logic channels.  The size is specialized
   to the size of the logic analyzer to keep the file size reasonable.
   It doesn't make much sense to go below 8 bits, since that requires
   a different indexing mechanism. */

typedef uint8_t NS(_t);

static inline NS(_t) NS(_max)(NS(_t) a, NS(_t) b) { return a | b; }
static inline NS(_t) NS(_min)(NS(_t) a, NS(_t) b) { return a & b; }

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
