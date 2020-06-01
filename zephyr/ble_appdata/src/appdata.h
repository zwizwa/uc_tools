#ifndef APPDATA_H
#define APPDATA_H

#include "spec.h"

#ifndef EMU
#include <bluetooth/gatt.h>
#endif


/* Generic appdata sum type.
   This will be wrapped in two layers of metadata:
   - struct characteristic annotates get/set of appdata
   - struct service collects multiple characteristics.
   All metadat ais const an can go into Flash storage.

   Note that the indexing wrapper struct server isn't strictly
   necessary.  In principle the structure could be reconstructed from
   bt_gatt_service_static's user_data pointers, which will point to
   struct characteristic.  However, struct service is a little easier
   to use.
&*/
union appdata {
    uint32_t u32;
    uint16_t u16;
    uint8_t u8;
    float32_t float32;
    uint8_t raw[4];
};

/* Reuse the bluetooth/uuid.h structures.  Note that this has some
   Flash overhead in case the uids are 16 bit, but that doesn't really
   seem to be a problem. */
union appdata_uuid {
    struct bt_uuid     uuid;
    struct bt_uuid_16  uuid_16;
    struct bt_uuid_128 uuid_128;
};

/* All access is abstract through get/set methods. */
typedef void                 (*appdata_set_fn)(const union appdata *value);
typedef const union appdata* (*appdata_get_fn)(void);

struct appdata_characteristic {
    union appdata_uuid uuid;
    appdata_get_fn get;
    appdata_set_fn set;
    uint16_t len;
    const char *desc;
    const char *type;
};
struct appdata_service {
    union appdata_uuid uuid;
    const char *desc;
    const struct appdata_characteristic **characteristics;
};

typedef void (*with_appdata_fn)(void *, const struct appdata_service *, const struct appdata_characteristic *);
void for_appdatas(const struct appdata_service **, with_appdata_fn fn, void *ctx);
void printk_services(const struct appdata_service **);

#ifndef EMU
ssize_t appdata_read(
    struct bt_conn *conn, const struct bt_gatt_attr *attr,
    void *buf, u16_t len, u16_t offset);

ssize_t appdata_write(
    struct bt_conn *conn, const struct bt_gatt_attr *attr,
    const void *buf, u16_t len, u16_t offset, u8_t flags);
#endif

#endif
