/* Basic system interface for C modules used on bare metal and Linux.
   Only LOG is custom atm.  The rest is defined in macros.h */

// FIXME: phase this out?

#ifndef UC_TOOLS_H
#define UC_TOOLS_H

#ifdef __linux__
#define UC_TOOLS_SYSTEM_IO_STDLIB
#endif

#ifdef UC_TOOLS_SYSTEM_IO_STDLIB

#ifndef LOG
#include <stdio.h>
#define LOG(...) fprintf(stderr, __VA_ARGS__)

#else

#ifndef LOG
#include "info.h"
#define LOG(...) infof(__VA_ARGS__)

#include "macros.h"


#endif
