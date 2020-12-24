#ifndef LOG_H
#define LOG_H

/* Abstract logger.

   Note that LOG is already defined in macros.h, but for some reason
   we cannot include infof.h in macros.h due to it influencing what is
   linked into the bootloader.  However, logging is needed almost
   everywhere, so dedicate a header file for that.

*/

/* This defines LOG */
#include "macros.h"

/* This ensures that infof is declares on non-linux platforms. */
#ifndef __linux__
#include "infof.h"
#endif

#endif
