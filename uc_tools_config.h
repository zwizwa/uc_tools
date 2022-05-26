#ifndef UC_TOOLS_CONFIG_H
#define UC_TOOLS_CONFIG_H

/* To avoid surprises, all code that is parameterized by compile-time
   debug functionality that does things like adding extra members to
   structs should be defined here. */

#ifndef CBUF_DEBUG
#define CBUF_DEBUG 1
#endif


#define CBUF_DEBUG_INFO_OVERFLOW 0

#define PBUF_DEBUG_FIELDS 1


#endif
