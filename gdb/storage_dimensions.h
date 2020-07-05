#ifndef STORAGE_DIMENSIONS_H
#define STORAGE_DIMENSIONS_H

#include "generic.h"

/* Naming scheme used in firmware:
   - block   = firmware logical block size, used for organization
   - cluster = device erase unit = recording start alignment
   - unit    = device program unit

   All sizes need to be derived from these defines.  It should be
   possible to increase the firmware block size without breaking
   anything.

   If you need to ensure a certain physical size, use a CT_ASSERT.

*/



// FIXME: also derive Erlang constants from this.

// PHYSICAL
#define SD_UNIT_LOGSIZE 9          // SDHC base unit
#define SD_UNITS_PER_CLUSTER 8192  // 4MByte suggested allocation unit

// CONVENTION
#define BLOCK_LOGSIZE 11  // param.gen.h

// Constraints
CT_ASSERT(sd_fits,   BLOCK_LOGSIZE >= SD_UNIT_LOGSIZE);

// Derived
#define SD_CLUSTER_SIZE   (SD_UNITS_PER_CLUSTER   << SD_UNIT_LOGSIZE)

#define SD_UNIT_SIZE      (1 << SD_UNIT_LOGSIZE)
#define BLOCK_SIZE        (1 << BLOCK_LOGSIZE)

#define SD_BLOCKS_PER_CLUSTER   (SD_CLUSTER_SIZE   >> BLOCK_LOGSIZE)

#define SD_UNITS_PER_BLOCK      (BLOCK_SIZE >> SD_UNIT_LOGSIZE)
#endif

