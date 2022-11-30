#ifndef MOD_TEST_I2C_MS
#define MOD_TEST_I2C_MS
/* I2C bus with two ports attached. */
/* Note that simulating the wire-and requires some effort.

   We need to keep track of the output state of each of the I2C ports,
   and combine the values on every read. */

struct i2c_port {
    uint8_t bits;
};
struct i2c_ports {
    struct i2c_port m, s;
} i2c_ports;
void i2c_ports_init(struct i2c_ports *p) {
    // Both high impedance.
    p->m.bits = 0b11;
    p->s.bits = 0b11;
}

int i2c_read_bus(void) {
    return i2c_ports.m.bits & i2c_ports.s.bits;
}
void i2c_write_port(struct i2c_port *port, uint8_t mask, int bitval) {
    port->bits &= ~mask;
    if (bitval) port->bits |= mask;
}

#define I2C_SCL (1<<1)
#define I2C_SDA (1<<0)

static inline int i2c_read_sda(struct i2c_port *s) {
    return !!(i2c_read_bus() & I2C_SDA);
}
static inline int i2c_read_scl(struct i2c_port *s) {
    return !!(i2c_read_bus() & I2C_SCL);
}
static inline void i2c_write_sda(struct i2c_port *s, int bitval) {
    i2c_write_port(s, I2C_SDA, bitval);
}
static inline void i2c_write_scl(struct i2c_port *s, int bitval) {
    i2c_write_port(s, I2C_SCL, bitval);
}

#endif
