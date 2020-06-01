/* Instantiation macros for spec.h tables. */

// LICENCE: Apache-2.0

#ifndef SPEC_IMPL_H
#define SPEC_IMPL_H

/* These are referenced in spec.h and implemented in terms of Zephyr's
   bluetooth/uuid.h */
#define UUID_16(_uuid)  {.uuid_16  = BT_UUID_INIT_16(_uuid) }

/* It is a pain to read 128 bit uids backwards, so this does the byte
   reversal. That keeps the spec.h more readable. */
#define UUID_128(b15,b14,b13,b12,b11,b10,b9,b8,b7,b6,b5,b4,b3,b2,b1,b0) \
    {.uuid_128 = BT_UUID_INIT_128(b0,b1,b2,b3,b4,b5,b6,b7,b8,b9,b10,b11,b12,b13,b14,b15) }

#include "spec.h"
#include "appdata.h"

/* 1. Map the spec.h tables (compile time iterators) to appdata
      structs. All metadata is const, and data for each characteristic
      is accessed abstractly through get/set methods. */

#define MAYBE_INIT_SET_APPDATA_RW(_cname)  .set = set_##_cname,
#define MAYBE_INIT_SET_APPDATA_RWN(_cname) .set = set_##_cname,
#define MAYBE_INIT_SET_APPDATA_R(_cname)
#define MAYBE_INIT_SET_APPDATA_RN(_cname)

// FIXME: support 128 bit.  This has some silly double union wrapping.
#define INIT_UUID(_uuid) {.uuid_16 = BT_UUID_INIT_16(_uuid)}

#define DEF_CHARACTERISTIC(_cname, _desc, _char_uuid, _type, _cap, _notes) \
    const struct appdata_characteristic characteristic_##_cname = {     \
        .get     = get_##_cname,                                        \
        .set     = set_##_cname,                                        \
        .desc    = _desc,                                               \
        .uuid    = _char_uuid,                                          \
        .len     = sizeof(_type),                                       \
        .type    = #_type,                                              \
        MAYBE_INIT_SET_APPDATA_##_cap(_cname)                           \
    };

#define REF_CHARACTERISTIC(_cname, _desc, _char_uuid, _type, _cap, _notes) \
    &characteristic_##_cname,

#define DEF_SERVICE(_cname, _desc, _serv_uuid, _FOR_CHARACTERISTICS)    \
    _FOR_CHARACTERISTICS(DEF_CHARACTERISTIC)                            \
    const struct appdata_characteristic *characteristics_##_cname[] = { \
        _FOR_CHARACTERISTICS(REF_CHARACTERISTIC) NULL                   \
    };                                                                  \
    const struct appdata_service service_##_cname = {                   \
        .desc = _desc,                                                  \
        .uuid = _serv_uuid,                                             \
        .characteristics = characteristics_##_cname                     \
    };
#define REF_SERVICE(_cname, _desc, _serv_uuid, _FOR_CHARACTERISTICS)    \
    &service_##_cname,

// Expand this in main.c
#define INSTANTIATE_APPDATA \
    FOR_SERVICES(DEF_SERVICE) \
    const struct appdata_service *services[] = { FOR_SERVICES(REF_SERVICE) NULL };


/* 2. Create declarations for all accessors. */

#define DECL_CHARACTERISTIC(_cname, _desc, _char_uuid, _type, _cap, _notes) \
    void set_##_cname(const union appdata *value);                      \
    const union appdata *get_##_cname(void);
#define DECL_SERVICE(_cname, _desc, _serv_uuid, _FOR_CHARACTERISTICS)   \
    _FOR_CHARACTERISTICS(DECL_CHARACTERISTIC)

FOR_SERVICES(DECL_SERVICE)


/* 3. If get/set doesn't need to be abstract, these can be used to
      create vars and get/set accessors. */

#define DEF_VAR(_var)                           \
    union appdata _var;
#define DEF_GET_SET(_cname, _var)                                       \
    void set_##_cname(const union appdata *value) {                     \
        *((union appdata *)(_var)) = *value;                            \
    }                                                                   \
    const union appdata *get_##_cname(void) {                           \
        return _var;                                                    \
    }
#define DEF_VAR_GET_SET(_cname)                                 \
    DEF_VAR(var_##_cname) DEF_GET_SET(_cname, &var_##_cname)


/* 4. BT_GATT_ abstractions.  For each capability type, there is a
      corresponding macro that can be spliced into the service
      definition. */

#define CHARACTERISTIC_RWN(_characteristic)                             \
    BT_GATT_CHARACTERISTIC(                                             \
        &_characteristic.uuid.uuid,                                     \
        BT_GATT_CHRC_READ | BT_GATT_CHRC_NOTIFY | BT_GATT_CHRC_WRITE,   \
        BT_GATT_PERM_READ | BT_GATT_PERM_WRITE,                         \
        appdata_read,                                                   \
        appdata_write,                                                  \
        (void*)&_characteristic),                                       \
    BT_GATT_CCC(                                                        \
        characteristic_ccc_cfg_changed,                                 \
        BT_GATT_PERM_READ | BT_GATT_PERM_WRITE_ENCRYPT)

#define CHARACTERISTIC_RN(_characteristic)                              \
    BT_GATT_CHARACTERISTIC(                                             \
        &_characteristic.uuid.uuid,                                     \
        BT_GATT_CHRC_READ | BT_GATT_CHRC_NOTIFY,                        \
        BT_GATT_PERM_READ,                                              \
        appdata_read,                                                   \
        NULL,                                                           \
        (void*)&_characteristic),                                       \
    BT_GATT_CCC(                                                        \
        characteristic_ccc_cfg_changed,                                 \
        BT_GATT_PERM_READ | BT_GATT_PERM_WRITE_ENCRYPT)

#define CHARACTERISTIC_R(_characteristic)                               \
    BT_GATT_CHARACTERISTIC(                                             \
        &_characteristic.uuid.uuid,                                     \
        BT_GATT_CHRC_READ,                                              \
        BT_GATT_PERM_READ,                                              \
        appdata_read,                                                   \
        NULL,                                                           \
        (void*)&_characteristic)                                        \

#define CHARACTERISTIC_RW(_characteristic)                              \
    BT_GATT_CHARACTERISTIC(                                             \
        &_characteristic.uuid.uuid,                                     \
        BT_GATT_CHRC_READ | BT_GATT_CHRC_WRITE,                         \
        BT_GATT_PERM_READ | BT_GATT_PERM_WRITE,                         \
        appdata_read,                                                   \
        appdata_write,                                                  \
        (void*)&_characteristic)

#define CHARACTERISTIC(_cname, _desc, _char_uuid, _type, _cap, _notes) \
    ,CHARACTERISTIC_##_cap(characteristic_##_cname)

// Note: referenceing uuid.uuid_16.uuid or uuid.uuid_128.uuid has the
// same effect.  FIXME: re-arrange this so the double wrapping is not
// necessary.

#define SERVICE(_cname, _desc, _serv_uuid, _FOR_CHARACTERISTICS) \
    BT_GATT_SERVICE_DEFINE(\
        _cname,\
        BT_GATT_PRIMARY_SERVICE((void*)&service_##_cname.uuid.uuid)\
        _FOR_CHARACTERISTICS(CHARACTERISTIC));

// Expand this in main.c
#define INSTANTIATE_BT_SERVICES FOR_SERVICES(SERVICE)

#endif
