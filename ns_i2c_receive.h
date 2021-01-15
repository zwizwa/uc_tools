// See comment ns_i2c_transmit.h


    //HW_I2C_WHILE ((I2C_SR2(c.i2c) & I2C_SR2_BUSY));


    /* Send start */
    I2C_CR1(c.i2c) |= I2C_CR1_START;

    /* Wait for master mode selected */
    HW_I2C_WHILE (!((I2C_SR1(c.i2c) & I2C_SR1_SB) &
             (I2C_SR2(c.i2c) & (I2C_SR2_MSL | I2C_SR2_BUSY)))) {
        if (!s->tries--) {
            s->sr = 0x30011;
            goto error;
        }
    }

    /* Send 7bit address */
    I2C_DR(c.i2c) = (uint8_t)((s->slave << 1) | I2C_READ);

    /* Enable ack */
    I2C_CR1(c.i2c) |= I2C_CR1_ACK;

    /* Wait for address bit done or error. */
    HW_I2C_WHILE(!(s->sr = I2C_SR1(c.i2c))) {
        if (!s->tries--) {
            s->sr = 0x30012;
            goto error;
        }
    }
    if (!(s->sr & I2C_SR1_ADDR)) {
        infof("i2c receive error waiting for addr: SR1=%x\n", s->sr);
        return s->sr;
    }

    /* Clearing ADDR condition sequence. */
    (void)I2C_SR2(c.i2c);

    for (s->i = 0; s->i < s->data_len; ++(s->i)) {
        if (s->i == s->data_len-1) {
            /* Disable ack */
            I2C_CR1(c.i2c) &= ~I2C_CR1_ACK;
        }

        HW_I2C_WHILE (!(s->sr = I2C_SR1(c.i2c))) {
            if (!s->tries--) {
                s->sr = 0x30013;
                goto error;
            }
        }
        if (!(I2C_SR1(c.i2c) & I2C_SR1_RxNE)) {
            infof("i2c receive data byte %d: SR1=%x\n", s->i, s->sr);
        }
        s->data[s->i] = I2C_DR(c.i2c) & 0xff;
    }

    return 0;
error:
    infof("i2c receive error: SR1=%x\n", s->sr);
    return s->sr;

