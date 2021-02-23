#ifndef MOD_MINMAX
#define MOD_MINMAX

#include "mmap_file.h"

#include <stdint.h>

#define NS(name) minmax##name
typedef int16_t NS(_t);
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
