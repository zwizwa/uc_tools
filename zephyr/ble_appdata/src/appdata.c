#include "appdata.h"

/* Generic read/write routines for appdatas.  These assume that
   attr->user_data is of type struct appdata. */

#ifndef EMU
ssize_t appdata_read(
    struct bt_conn *conn, const struct bt_gatt_attr *attr,
    void *buf, uint16_t len, uint16_t offset) {
    const struct appdata_characteristic *c = attr->user_data;
    const union appdata *appdata = c->get ? c->get() : NULL;

    if (appdata) {
        printk("appdata_read: %s (len=%d)\n", c->desc, c->len);
        return bt_gatt_attr_read(
            conn, attr,
            buf, len, offset,
            appdata,
            c->len);
    }
    else {
        printk("appdata_read: no getter\n");
        return 0;
    }
}
ssize_t appdata_write(
    struct bt_conn *conn, const struct bt_gatt_attr *attr,
    const void *buf, uint16_t len, uint16_t offset, uint8_t flags) {

    struct appdata_characteristic *c = attr->user_data;
    /* Only support whole value writes. */
    if (offset != 0) {
        printk("appdata_write: bad offset %d\n", offset);
        return BT_GATT_ERR(BT_ATT_ERR_INVALID_OFFSET);
    }
    if (len != c->len) {
        printk("appdata_write: bad len %d\n", len);
        return BT_GATT_ERR(BT_ATT_ERR_INVALID_ATTRIBUTE_LEN);
    }
    // FIXME: Implement ranges and throw BT_ATT_ERR_VALUE_NOT_ALLOWED?
    if (c->set) {
        printk("appdata_write: %s (len=%d)\n", c->desc, c->len);
        c->set(buf);
        return len;
    }
    else {
        printk("appdata_write: no setter\n");
        // FIXME :ERROR
        return 0;
    }
}
#endif

void for_appdatas(const struct appdata_service **services,
                  with_appdata_fn fn, void *ctx) {
    for (const struct appdata_service **s = services; *s; s++) {
        for (const struct appdata_characteristic **c = (*s)->characteristics; *c; c++) {
            fn(ctx, *s, *c);
        }
    }
}


void printk_appdata(void *ctx,
                    const struct appdata_service *s,
                    const struct appdata_characteristic *c) {
    // FIXME: this assumes 16-bit uuid, e.g.:
    //if (s->uuid.uuod == BT_UUID_TYPE_16) {}
    //if (c->uuid.uuod == BT_UUID_TYPE_16) {}
    printk("%04x %04x %s (%s)\n",
           s->uuid.uuid_16.val,
           c->uuid.uuid_16.val,
           c->desc, c->type);
}
void printk_services(const struct appdata_service **services) {
    for_appdatas(services, printk_appdata, NULL);
}


