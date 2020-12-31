#ifndef LET_H
#define LET_H

/* A-normal form for dataflow networks.

   Currently thinking to keep this _really_ simple:

   - don't separate signal output and state output structs

   - require all init to be zero

   - require named inputs

   - every instance has a const config type, which can be NULL

   This will probably bite later, but let's just give it a try.
*/
#define LET_STATIC(_instance_name,_type_name,_config_ptr,...)           \
    static _type_name##_state _instance_name = {};                      \
    const  _type_name##_input _instance_name##_input = { __VA_ARGS__ }; \
    _type_name##_update(_config_ptr, &_instance_name, &_instance_name##_input)

#define LET LET_STATIC

#endif
