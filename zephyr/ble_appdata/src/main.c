/* Example for appdata.h characteristic abstraction layer. */

#include "appdata.h"

#include <zephyr/types.h>
#include <stddef.h>
#include <string.h>
#include <errno.h>
#include <sys/printk.h>
#include <sys/byteorder.h>
#include <zephyr.h>

#include <settings/settings.h>

#include <bluetooth/bluetooth.h>
#include <bluetooth/hci.h>
#include <bluetooth/conn.h>
#include <bluetooth/uuid.h>
#include <bluetooth/gatt.h>

#include <stdint.h>

#include "spec_impl.h"

/* Instantiate a union appdata variable for each of these explicitly,
   together with get/set functions.  Later these can be replaced by
   abstract get/set functions if needed.

   The get/set functions are referenced by the metatdata instantiation
   below. */
DEF_VAR_GET_SET(char11)
DEF_VAR_GET_SET(char12)
DEF_VAR_GET_SET(char13)
DEF_VAR_GET_SET(char21)

/* Currently all config change messages go through this function.
   This might not be how it is supposed to be...  The function is
   referenced in the instantation below, so defined it first. */
static void characteristic_ccc_cfg_changed(const struct bt_gatt_attr *attr, u16_t value) {
    printk("ccc %p %d\n", attr, value);
}

/* Instantiate appdata.h metadata structs and BT services. */
INSTANTIATE_APPDATA
INSTANTIATE_BT_SERVICES

/* Now we just need to advertise the services. */
static const struct bt_data ad[] = {
    BT_DATA_BYTES(
        BT_DATA_FLAGS,
        (BT_LE_AD_GENERAL | BT_LE_AD_NO_BREDR)),
    BT_DATA_BYTES(
        BT_DATA_UUID16_ALL,
        // All services
        0xA0, 0xFF,
        0xB0, 0xFF,
        0xC0, 0xFF,
        0xD0, 0xFF,
        0xe0, 0xFF),
};

static void connected(struct bt_conn *conn, u8_t err) {
    if (err) {
        printk("Connection failed (err 0x%02x)\n", err);
    } else {
        printk("Connected\n");
    }
}
static void disconnected(struct bt_conn *conn, u8_t reason) {
    printk("Disconnected (reason 0x%02x)\n", reason);
}
static struct bt_conn_cb conn_callbacks = {
	.connected = connected,
	.disconnected = disconnected,
};
static void start_bluetooth(void) {
    int err;
    if ((err = bt_enable(NULL))) {
        printk("Bluetooth init failed (err %d)\n", err);
        return;
    }
    printk("Bluetooth initialized\n");

    /* This is necessary.  Otherwise error -11 is returned. */
    if (IS_ENABLED(CONFIG_SETTINGS)) { settings_load(); }

    err = bt_le_adv_start(BT_LE_ADV_CONN_NAME, ad, ARRAY_SIZE(ad), NULL, 0);
    if (err) {
        printk("Advertising failed to start (err %d)\n", err);
        return;
    }
    printk("Advertising successfully started\n");
}

static void auth_passkey_display(struct bt_conn *conn, unsigned int passkey) {
    char addr[BT_ADDR_LE_STR_LEN];
    bt_addr_le_to_str(bt_conn_get_dst(conn), addr, sizeof(addr));
    printk("Passkey for %s: %06u\n", addr, passkey);
}
static void auth_cancel(struct bt_conn *conn) {
    char addr[BT_ADDR_LE_STR_LEN];
    bt_addr_le_to_str(bt_conn_get_dst(conn), addr, sizeof(addr));
    printk("Pairing cancelled: %s\n", addr);
}
static struct bt_conn_auth_cb auth_cb_display = {
    .passkey_display = auth_passkey_display,
    .passkey_entry = NULL,
    .cancel = auth_cancel,
};


/* Search in the bt_gatt_service_static for our characteristic
   metadata, to obtain the correct bt_gatt_attr that needs to be
   passed to bt_gatt_notify.  Because all this is static, I cannot se
   a straightforward way to avoid this search. */
const struct bt_gatt_attr *find_characteristic(
    const struct bt_gatt_service_static *s,
    const struct appdata_characteristic *c) {
    for(int i=0; i<s->attr_count; i++) {
        const struct bt_gatt_attr *a = &s->attrs[i];
        if (a->user_data == c) { return a; }
    }
    return NULL;
}
void notify(const struct bt_gatt_service_static *s,
            const struct appdata_characteristic *c) {
    const struct bt_gatt_attr *attr = find_characteristic(s, c);
    if (!attr) {
        printk("no bt_gatt_service_static for characteristic %p\n", c);
        return;
    }
    const union appdata *data = c->get ? c->get() : 0;
    if (data) {
        //printk("notify %s\n", c->desc);
        bt_gatt_notify(NULL, attr, data, c->len);
    }
    else {
        printk("no data for characteristic %p\n", c);
    }
}


void main(void) {

    printk_services(services);

    start_bluetooth();
    bt_conn_cb_register(&conn_callbacks);
    bt_conn_auth_cb_register(&auth_cb_display);

    /* Implement notification. At the moment there is no suitable way
     * of starting delayed work so we do it here. */
    while (1) {

        k_sleep(K_SECONDS(1));

        notify(&serv1, &characteristic_char13);
        notify(&serv2, &characteristic_char21);

        union appdata a;

        a = *get_char13();
        a.u32++;
        set_char13(&a);

        a = *get_char21();
        a.u32++;
        set_char21(&a);

    }
}
