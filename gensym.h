#ifndef CONCAT
/* Identifier concatenation. */
#define _CONCAT(X,Y)  X##Y
#define CONCAT(X,Y)   _CONCAT(X,Y)
#endif

/* Per-file original symbol generation */
#ifndef GENSYM
#define GENSYM(sym)   CONCAT(sym,__COUNTER__)
#endif

