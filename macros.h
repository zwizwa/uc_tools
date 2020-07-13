// Where to put this file?  These macros are used everywhere.

// rename this file
#ifndef UC_TOOLS_MACROS_H
#define UC_TOOLS_MACROS_H

#include "gensym.h"


#ifndef ARRAY_SIZE
#define ARRAY_SIZE(x) \
    (sizeof(x)/sizeof(x[0]))
#endif

#define CT_ASSERT(name, pred) \
    typedef char nct_assert_##name[(pred) ? 1 : -1]
#define CT_ASSERT_STRUCT_SIZE(name, size) \
    CT_ASSERT(struct_##name, sizeof(struct name) == size)
#define CT_ASSERT_UNION_SIZE(name, size) \
    CT_ASSERT(union_##name, sizeof(union name) == size)




#endif // UC_TOOLS_MACROS_H

