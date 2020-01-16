// Where to put this file?  These macros are used everywhere.

// rename this file
#ifndef UC_TOOLS_MACROS_H
#define UC_TOOLS_MACROS_H

#include "gensym.h"


#ifndef ARRAY_SIZE
#define ARRAY_SIZE(x) \
    (sizeof(x)/sizeof(x[0]))
#endif


#endif // UC_TOOLS_MACROS_H

