#ifndef LET_H
#define LET_H

/* A-normal form for dataflow networks.

   Currently thinking to keep this _really_ simple:

   - don't separate signal output and state output structs

   - require named inputs

   - every instance has a const config type, which can be NULL

   - execution can be conditional, e.g. to implement evented systems
     as partial graph execution.


   Revisit?

   - require all init to be zero


*/
#define LET_COND(_cond,_instance_name,_type_name,_config_ptr,...)       \
    static _type_name##_state _instance_name = {};                      \
    if (_cond) {                                                        \
        const  _type_name##_input _instance_name##_input = { __VA_ARGS__ }; \
        _type_name##_update(_config_ptr, &_instance_name, &_instance_name##_input); \
    }

#define LET(...) LET_COND(1, __VA_ARGS__)

#endif
