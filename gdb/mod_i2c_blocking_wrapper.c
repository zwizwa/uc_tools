#ifndef MOD_I2C_BLOCKING_WRAPPER
#define MOD_I2C_BLOCKING_WRAPPER

/* this needs _init / _tick methods defined. */

/* Blocking wrappers. */
uint32_t i2c_transmit(
    uint32_t slave,
    const uint8_t *hdr, uint32_t hdr_len,
    const uint8_t *data, uint32_t data_len) {
    i2c_transmit_t s;
    i2c_transmit_init(&s, slave, hdr, hdr_len, data, data_len);
    while (SM_WAITING == i2c_transmit_tick(&s));
    return s.ctrl.sr;
}
uint32_t i2c_receive(
    uint32_t slave,
    uint8_t *data, uint32_t data_len) {
    i2c_receive_t s;
    i2c_receive_init(&s, slave, data, data_len);
    while (SM_WAITING == i2c_receive_tick(&s));
    return s.ctrl.sr;
}

void i2c_stop(void) {
    i2c_stop_t s;
    i2c_stop_init(&s);
    while (SM_WAITING == i2c_stop_tick(&s));
}


#endif
