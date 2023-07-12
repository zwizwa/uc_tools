#ifndef METASTRUCT_H
#define METASTRUCT_H

#include "macros.h"
#include <stdint.h>

/* STRUCTS */

#define STRUCT_FIELD(typ, nam) typ nam;

#define STRUCT_DEF(name, for_fields) \
    typedef struct { for_fields(STRUCT_FIELD) } name

#define STRUCT_CONST_DEF(name, for_fields) \
    typedef const struct { for_fields(STRUCT_FIELD) } name



/* META */

struct metastruct_field {
    const char *type;
    const char *name;
};
struct metastruct_struct {
    const struct metastruct_field *fields;
    uint32_t nb_fields;
};
#define METASTRUCT_STRUCT(name) { name##_fields, ARRAY_SIZE(name##_fields) }

#define METASTRUCT_FIELD(typ, nam) { #typ, #nam },
#define METASTRUCT_DEF(name, for_fields) \
    const struct metastruct_field name##_fields[] = {            \
        for_fields(METASTRUCT_FIELD)                             \
    };                                                           \

#endif

