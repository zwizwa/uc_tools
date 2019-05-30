// Where to put this file?  These macros are used everywhere.

// rename this file
#ifndef UC_TOOLS_MACROS_H
#define UC_TOOLS_MACROS_H

#ifndef CONCAT
/* Identifier concatenation. */
#define _CONCAT(X,Y)  X##Y
#define CONCAT(X,Y)   _CONCAT(X,Y)
#endif

/* Per-file original symbol generation */
#ifndef GENSYM
#define GENSYM(sym)   CONCAT(sym,__COUNTER__)
#endif

#ifndef ARRAY_SIZE
#define ARRAY_SIZE(x) \
    (sizeof(x)/sizeof(x[0]))
#endif


#endif // UC_TOOLS_MACROS_H

