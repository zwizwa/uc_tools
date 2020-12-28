#ifndef LET_H
#define LET_H

/* A-normal form for dataflow networks.

   Currently thinking to keep this _really_ simple:

   - don't separating signal output and state output

   - require all init to be zero

   This will probably bite later, but let's just give it a try.
*/
#define LET_STATIC(_instance_name,_type_name,  ...) \
    static _type_name##_t _instance_name = {}; \
    _type_name##_update(&_instance_name, __VA_ARGS__)

#define LET_STRUCT_MEMBER(_instance_name, _type_name,  ...) \
    _type_name _instance_name

#define LET_STRUCT_REF(_instance_name, _type_name, ...) \
    _type_name##_update(&_instance_name, __VA_ARGS__)



//#define LET LET_STATIC

#endif
