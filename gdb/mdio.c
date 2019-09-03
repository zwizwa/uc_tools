
#include "mdio.h"

#define MDIO_READ  1
#define MDIO_WRITE 0

static void send_bit(int val) {
    mdio_set_data(val);
    mdio_delay();
    mdio_set_clock(1);
    mdio_delay();
    mdio_set_clock(0);
}
static int get_bit(void) {
    mdio_delay();
    mdio_set_clock(1);
    mdio_delay();
    mdio_set_clock(0);
    return mdio_get_data();
}
static void send_num(uint16_t val, int bits) {
    int i;
    for (i=bits-1; i>=0; i--) {
        send_bit((val >> i) & 1);
    }
}
 uint16_t get_num(int bits) {
    int i;
    uint16_t ret = 0;
    for (i=bits - 1; i>=0; i--) {
        ret <<= 1;
        ret |= get_bit();
    }
    return ret;
}
static void send_preamble(void) {
    for (int i = 0; i < 32; i++) {
        send_bit(1);
    }
}
static void read_flush(void) {
    for (int i = 0; i < 32; i++) {
        get_bit();
    }
}
static void cmd(int read, uint8_t phy, uint8_t reg) {
    mdio_set_dir(1);
    send_preamble();
    send_bit(0);
    send_bit(1);
    send_bit(read);
    send_bit(!read);
    send_num(phy, 5);
    send_num(reg, 5);
}
uint16_t mdio_read(uint8_t phy, uint8_t reg) {
    int ret;
    cmd(MDIO_READ, phy, reg);
    mdio_set_dir(0);
    if (get_bit() != 0) {
        read_flush();
        return 0xffff;
    }
    ret = get_num(16);
    get_bit();
    return ret;
}
void mdio_write(uint8_t phy, uint8_t reg, uint16_t val) {
    cmd(MDIO_WRITE, phy, reg);
    /* turnaround (10) */
    send_bit(1);
    send_bit(0);
    send_num(val, 16);
    mdio_set_dir(0);
    get_bit();
}

