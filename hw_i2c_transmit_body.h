/* FIXME: This is a new pattern, necessary to parameterize the body of
   a function against some macros.  Currently I have to move fast to
   get something going, but clean this up a bit!

   The main reason to put it in a separate file is that the code is
   too long to move it into a macro, so use #include to inline the
   code.
*/

/* This uses the following context:
   S is a pointer to hw_i2c_transmit struct
   c is hw config
*/

    HW_I2C_WHILE ((I2C_SR2(c.i2c) & I2C_SR2_BUSY)) {
        if (!S->tries--) {
            S->sr = 0x30000;
            goto error;
        }
    }

    /* send start */
    I2C_CR1(c.i2c) |= I2C_CR1_START;

    /* Wait for master mode selected */
    HW_I2C_WHILE (!((I2C_SR1(c.i2c) & I2C_SR1_SB) &
             (I2C_SR2(c.i2c) & (I2C_SR2_MSL | I2C_SR2_BUSY)))) {
        if (!S->tries--) {
            S->sr = 0x30001;
            goto error;
        }
    }

    /* Send 7bit address */
    I2C_DR(c.i2c) = (uint8_t)((S->slave << 1) | I2C_WRITE);

    /* Wait for address bit done or error. */
    HW_I2C_WHILE(!(S->sr = I2C_SR1(c.i2c))) {
        if (!S->tries--) {
            S->sr = 0x30002;
            goto error;
        }
    }
    if (!(S->sr & I2C_SR1_ADDR)) goto error;

    /* Clearing ADDR condition sequence. */
    (void)I2C_SR2(c.i2c);

    if (S->hdr) {
        for (S->i = 0; S->i <S-> hdr_len; S->i++) {
            I2C_DR(c.i2c) = S->hdr[S->i];
            HW_I2C_WHILE (!(S->sr = I2C_SR1(c.i2c))) {
                if (!S->tries--) {
                    S->sr = 0x30003;
                    goto error;
                }
            }
            if (!(S->sr | I2C_SR1_BTF)) goto error;
        }
    }
    if (S->data) {
        for (S->i = 0; S->i < S->data_len; S->i++) {
            I2C_DR(c.i2c) = S->data[S->i];
            HW_I2C_WHILE (!(S->sr = I2C_SR1(c.i2c))) {
                if (!S->tries--) {
                    S->sr = 0x30004;
                    goto error;
                }
            }
            if (!(S->sr | I2C_SR1_BTF)) goto error;
        }
    }
    return 0;

  error:
    LOG("i2c transmit error: %x\n", S->sr);
    return S->sr;
