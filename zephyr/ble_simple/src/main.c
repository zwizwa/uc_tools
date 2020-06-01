

/* See also xo/zephyr/ble_example */


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

/* Single service with single attribute. */

/* These originally had 128 bit UIDs. According to Jeff that is really
 * not necessary, so they've been changed to 16 bit. */

static       struct bt_uuid_16 serv1_uuid       = BT_UUID_INIT_16(0xFFF0);
static const struct bt_uuid_16 serv1_char1_uuid = BT_UUID_INIT_16(0xFFF1);
static const struct bt_uuid_16 serv1_char2_uuid = BT_UUID_INIT_16(0xFFF2);

/* Generic characteristic sum type. */
struct characteristic {
    uint16_t type;
    uint16_t len;
    union {
        uint32_t u32;
        uint8_t data[4];
    } val;
    const char *desc;
};
static void characteristic_ccc_cfg_changed(const struct bt_gatt_attr *attr, u16_t value) {
    printk("ccc %p %d\n", attr, value);
}
static ssize_t characteristic_read(
    struct bt_conn *conn, const struct bt_gatt_attr *attr,
    void *buf, u16_t len, u16_t offset) {
    const struct characteristic *c = attr->user_data;
    printk("characteristic_read: %s (len=%d)\n", c->desc, c->len);
    return bt_gatt_attr_read(
        conn, attr,
        buf, len, offset,
        c->val.data,
        c->len);
}
static ssize_t characteristic_write(
    struct bt_conn *conn, const struct bt_gatt_attr *attr,
    const void *buf, u16_t len, u16_t offset, u8_t flags) {

    struct characteristic *c = attr->user_data;
    /* Only support whole value writes. */
    if (offset != 0) {
        printk("characteristic_write: bad offset %d\n", offset);
        return BT_GATT_ERR(BT_ATT_ERR_INVALID_OFFSET);
    }
    if (len != c->len) {
        printk("characteristic_write: bad len %d\n", len);
        return BT_GATT_ERR(BT_ATT_ERR_INVALID_ATTRIBUTE_LEN);
    }
    // FIXME: Implement ranges and throw BT_ATT_ERR_VALUE_NOT_ALLOWED?
    memcpy(c->val.data, buf, len);
    printk("characteristic_write: %s (len=%d)\n", c->desc, c->len);
    return len;
}

/* Instance */
static struct characteristic serv1_char1_value = {
    .type = 0, // FIXME
    .len  = 4,
    .val = {.u32 = 0 },
    .desc = "char1",
};
static struct characteristic serv1_char2_value = {
    .type = 0, // FIXME
    .len  = 4,
    .val = {.u32 = 0 },
    .desc = "char2",
};

/* This is fucked up. BT_GATT_CHARACETERISTIC expands to 2 attributes.

/* Vendor Primary Service Declaration */
BT_GATT_SERVICE_DEFINE(
    serv1,
    //0
    BT_GATT_PRIMARY_SERVICE(
        &serv1_uuid),
    //1,2
    BT_GATT_CHARACTERISTIC(
        &serv1_char1_uuid.uuid,
        BT_GATT_CHRC_READ | BT_GATT_CHRC_NOTIFY | BT_GATT_CHRC_WRITE,
        BT_GATT_PERM_READ | BT_GATT_PERM_WRITE,
        characteristic_read,
        characteristic_write,
        &serv1_char1_value),
    //3
    BT_GATT_CCC(
        characteristic_ccc_cfg_changed,
        BT_GATT_PERM_READ | BT_GATT_PERM_WRITE_ENCRYPT),
    //4,5
    BT_GATT_CHARACTERISTIC(
        &serv1_char2_uuid.uuid,
        BT_GATT_CHRC_READ | BT_GATT_CHRC_NOTIFY | BT_GATT_CHRC_WRITE,
        BT_GATT_PERM_READ | BT_GATT_PERM_WRITE,
        characteristic_read,
        characteristic_write,
        &serv1_char2_value),
    //6
    BT_GATT_CCC(
        characteristic_ccc_cfg_changed,
        BT_GATT_PERM_READ | BT_GATT_PERM_WRITE_ENCRYPT),
    );

static const struct bt_data ad[] = {
    BT_DATA_BYTES(
        BT_DATA_FLAGS,
        (BT_LE_AD_GENERAL | BT_LE_AD_NO_BREDR)),
    BT_DATA_BYTES(
        BT_DATA_UUID16_ALL,
        0xF0, 0xFF),   // Custom
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

    /* It won't work without this.  Gives: -11 error otherwise. */
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


void main(void) {
    start_bluetooth();
    bt_conn_cb_register(&conn_callbacks);
    bt_conn_auth_cb_register(&auth_cb_display);

    /* Implement notification. At the moment there is no suitable way
     * of starting delayed work so we do it here. */
    while (1) {

        k_sleep(K_SECONDS(1));

        bt_gatt_notify(
            NULL, &serv1.attrs[1],
            &serv1_char1_value.val.data,
            serv1_char1_value.len);
        serv1_char1_value.val.u32++;

        bt_gatt_notify(
            NULL, &serv1.attrs[4],
            &serv1_char2_value.val.data,
            serv1_char2_value.len);
        serv1_char2_value.val.u32+=3;

    }
}
