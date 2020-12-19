#ifndef LET_H
#define LET_H

/* A-normal form for dataflow networks.

   Currently thinking to keep this _really_ simple:

   - don't separating signal output and state output

   - require all init to be zero

   This will probably bite later, but let's just give it a try.
*/
#define LET_STATIC(typ, nam, ...) \
    static typ nam = {}; typ##_update(&nam, __VA_ARGS__)


#define LET_STRUCT_MEMBER(typ, nam, ...) \
    typ nam
#define LET_STRUCT_REF(typ, nam, ...) \
    typ##_update(&nam, __VA_ARGS__)

#define LET LET_STATIC

#endif
