/* FIXME: This is a new pattern, necessary to parameterize the body of
   a function against some macros.  Currently I have to move fast to
   get something going, but clean this up a bit!

   The main reason to put it in a separate file is that the code is
   too long to move it into a macro, so use #include to inline the
   code.
*/

    HW_I2C_WHILE ((I2C_SR2(c.i2c) & I2C_SR2_BUSY)) {
        if (!s->tries--) {
            s->sr = 0x30000;
            goto error;
        }
    }

    /* send start */
    I2C_CR1(c.i2c) |= I2C_CR1_START;

    /* Wait for master mode selected */
    HW_I2C_WHILE (!((I2C_SR1(c.i2c) & I2C_SR1_SB) &
             (I2C_SR2(c.i2c) & (I2C_SR2_MSL | I2C_SR2_BUSY)))) {
        if (!s->tries--) {
            s->sr = 0x30001;
            goto error;
        }
    }

    /* Send 7bit address */
    I2C_DR(c.i2c) = (uint8_t)((s->slave << 1) | I2C_WRITE);

    /* Wait for address bit done or error. */
    HW_I2C_WHILE(!(s->sr = I2C_SR1(c.i2c))) {
        if (!s->tries--) {
            s->sr = 0x30002;
            goto error;
        }
    }
    if (!(s->sr & I2C_SR1_ADDR)) goto error;

    /* Clearing ADDR condition sequence. */
    (void)I2C_SR2(c.i2c);

    if (s->hdr) {
        for (s->i = 0; s->i <s-> hdr_len; s->i++) {
            I2C_DR(c.i2c) = s->hdr[s->i];
            HW_I2C_WHILE (!(s->sr = I2C_SR1(c.i2c))) {
                if (!s->tries--) {
                    s->sr = 0x30003;
                    goto error;
                }
            }
            if (!(s->sr | I2C_SR1_BTF)) goto error;
        }
    }
    if (s->data) {
        for (s->i = 0; s->i < s->data_len; s->i++) {
            I2C_DR(c.i2c) = s->data[s->i];
            HW_I2C_WHILE (!(s->sr = I2C_SR1(c.i2c))) {
                if (!s->tries--) {
                    s->sr = 0x30004;
                    goto error;
                }
            }
            if (!(s->sr | I2C_SR1_BTF)) goto error;
        }
    }
    return 0;

  error:
    LOG("i2c transmit error: %x\n", s->sr);
    return s->sr;
