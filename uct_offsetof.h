#ifndef UCT_OFFSETOF_H
#define UCT_OFFSETOF_H

#include "macros.h"
#define DEF_FIELD_TO_PARENT(function_name, parent_type, field_type, field_name) \
    static inline parent_type *function_name(field_type *field_ptr) {   \
        uint8_t *u8 = ((uint8_t *)field_ptr) - OFFSETOF(parent_type,field_name); \
        return (parent_type *)u8;                                       \
    }

#endif
