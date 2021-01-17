#ifndef VL53L1X_H
#define VL53L1X_H

/* Minimal driver, flattened from
   https://github.com/sparkfun/SparkFun_VL53L1X_Arduino_Library

   Which is in turn derived from VL53L1X ULD API (Ultra Lite Driver
   Application Programming Interface) accessible from the STM product
   page.

   Original STM code allows choice between proprietary STM license,
   and BSD 3-clause, which is what we use here. */

/* COPYRIGHT(c) 2018 STMicroelectronics
 *
 * Redistribution and use in source and binary forms, with or without modification,
 * are permitted provided that the following conditions are met:
 *   1. Redistributions of source code must retain the above copyright notice,
 *      this list of conditions and the following disclaimer.
 *   2. Redistributions in binary form must reproduce the above copyright notice,
 *      this list of conditions and the following disclaimer in the documentation
 *      and/or other materials provided with the distribution.
 *   3. Neither the name of STMicroelectronics nor the names of its contributors
 *      may be used to endorse or promote products derived from this software
 *      without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
 * OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

/* The HAL used here is based on these symbols being defined externally:
   hw_i2c_ ...
*/

/* Original Arduino code used only:
   .begin()
   .startRanging()
   .checkForDataReady()
   .getDistance()
   .clearInterrupt()
   .stopRanging()
*/

#include "byteswap.h"

struct vl53l1x {
    /* Code has been simplified to optimize for happy path.  Errors
       are passed in context struct instead of as return value. Return
       value is valid only when s->status == 0. That makes code a lot
       easier to read, and creates more compact binaries. */
    int status;
};


/* The HAL uses macros instead of function pointers to avoid function
   pointer red tape. */

#ifndef VL51L1X_HAL_I2C_TRANSIMIT
#error need VL51L1X_HAL_I2C_TRANSIMIT
#endif

#ifndef VL51L1X_HAL_I2C_RECEIVE
#error need VL51L1X_HAL_I2C_RECEIVE
#endif

/* Stop condition is separate in uc_tools HAL to allow restarts, but
   we need to send stop.  Chip does not use restart. */
#ifndef VL51L1X_HAL_I2C_STOP
#error need VL51L1X_HAL_I2C_STOP
#endif


#ifndef VL51L1X_HAL_WAIT_MS
#error need VL51L1X_HAL_WAIT_MS
#endif

/* Driver assumes this is known at compile time.  The convention used
   here is what is provided to the original SetInterruptPolarity
   routine, (1=active high, 0=active low), which sets the inverse of
   bit 4 in GPIO_HV_MUX__CTRL. */
#ifndef VL53L1X_INTERRUPT_POLARITY
#define VL53L1X_INTERRUPT_POLARITY 1
#endif

/* Datasheet uses the convention that the R/W bit is part of the
   address, and states devices address as 0x52 for read and 0x53 for
   write.  We use the convention that address signifies the upper 7
   bits only, so shift by one. */
#define VL51L1X_I2C_ADDR (0x52>>1)

static inline void vl53l1x_write(struct vl53l1x *s, uint16_t index, uint8_t *buf, uint32_t len) {
    uint8_t header[2] = { U16_BE(index) };
    s->status =
        VL51L1X_HAL_I2C_TRANSIMIT(
            VL51L1X_I2C_ADDR,
            header, sizeof(header),
            buf, len);
    VL51L1X_HAL_I2C_STOP();
}
static inline void vl53l1x_read(struct vl53l1x *s, uint16_t index, uint8_t *buf, uint32_t len) {
    /* The device uses write followed by read.  No repeated start. */
    vl53l1x_write(s, index, 0, 0);
    if (s->status) goto stop;
    s->status =
        VL51L1X_HAL_I2C_RECEIVE(
            VL51L1X_I2C_ADDR,
            buf, len);
  stop:
    VL51L1X_HAL_I2C_STOP();
}

/* Only the ones that are actually used are copied here. */
#define VL53L1_IDENTIFICATION__MODEL_ID                       0x010F
#define VL53L1_VHV_CONFIG__TIMEOUT_MACROP_LOOP_BOUND          0x0008
#define SYSTEM__INTERRUPT_CLEAR                               0x0086
#define SYSTEM__MODE_START                                    0x0087
#define VL53L1_RESULT__FINAL_CROSSTALK_CORRECTED_RANGE_MM_SD0 0x0096
#define GPIO__TIO_HV_STATUS                                   0x0031


typedef int8_t vl53l1x_error_t;
#define VL53L1_ERROR_TIME_OUT  ((vl53l1x_error_t) - 7)


/* Register read. */
static inline uint32_t vl53l1x_rd_reg(struct vl53l1x *s, uint16_t index, uint32_t reg_size) {
    uint8_t buf[reg_size];
    vl53l1x_read(s, index, buf, reg_size);
    return read_be_32(buf, reg_size);
}
static inline uint16_t vl53l1x_rd_u8 (struct vl53l1x *s, uint16_t i) { return vl53l1x_rd_reg(s, i, 1); }
static inline uint16_t vl53l1x_rd_u16(struct vl53l1x *s, uint16_t i) { return vl53l1x_rd_reg(s, i, 2); }
static inline uint16_t vl53l1x_rd_u32(struct vl53l1x *s, uint16_t i) { return vl53l1x_rd_reg(s, i, 4); }

static inline void vl53l1x_wr_reg(struct vl53l1x *s, uint16_t index, uint32_t reg_size, uint32_t value) {
    uint8_t buf[reg_size];
    write_be_32(buf, value, reg_size);
    vl53l1x_write(s, index, buf, reg_size);
}
static inline void vl53l1x_wr_u8 (struct vl53l1x *s, uint16_t i, uint8_t v)  { vl53l1x_wr_reg(s, i, 1, v); }
static inline void vl53l1x_wr_u16(struct vl53l1x *s, uint16_t i, uint16_t v) { vl53l1x_wr_reg(s, i, 2, v); }
static inline void vl53l1x_wr_u32(struct vl53l1x *s, uint16_t i, uint32_t v) { vl53l1x_wr_reg(s, i, 4, v); }


static inline uint16_t vl53l1x_get_sensor_id (struct vl53l1x *s) { return vl53l1x_rd_u16(s, VL53L1_IDENTIFICATION__MODEL_ID); }
static inline uint16_t vl53l1x_get_distance  (struct vl53l1x *s) { return vl53l1x_rd_u16(s, VL53L1_RESULT__FINAL_CROSSTALK_CORRECTED_RANGE_MM_SD0); }

static inline uint8_t vl53l1x_check_for_data_ready(struct vl53l1x *s) {
    /* Driver assumes the input polarity is known at compile time.
       The original ST driver queries the chip fir this, which seems
       silly. */
    return (vl53l1x_rd_u8(s, GPIO__TIO_HV_STATUS) & 1) ^ (1^VL53L1X_INTERRUPT_POLARITY);
}

static inline void vl53l1x_clear_interrupt(struct vl53l1x *s) { vl53l1x_wr_u8 (s, SYSTEM__INTERRUPT_CLEAR, 1); }
static inline void vl53l1x_start_ranging  (struct vl53l1x *s) { vl53l1x_wr_u8 (s, SYSTEM__MODE_START, 0x40); }
static inline void vl53l1x_stop_ranging   (struct vl53l1x *s) { vl53l1x_wr_u8 (s, SYSTEM__MODE_START, 0x00); }


/* Contains busywait with 150ms timeout. Run this only at startup. */

#define VL51L1X_DEFAULT_CONFIGURATION_INIT { \
	0x00, /* 0x2d : set bit 2 and 5 to 1 for fast plus mode (1MHz I2C), else don't touch */ \
	0x01, /* 0x2e : bit 0 if I2C pulled up at 1.8V, else set bit 0 to 1 (pull up at AVDD) */ \
	0x01, /* 0x2f : bit 0 if GPIO pulled up at 1.8V, else set bit 0 to 1 (pull up at AVDD) */ \
	0x01, /* 0x30 : set bit 4 to 0 for active high interrupt and 1 for active low (bits 3:0 must be 0x1), use SetInterruptPolarity() */ \
	0x02, /* 0x31 : bit 1 = interrupt depending on the polarity, use CheckForDataReady() */ \
	0x00, /* 0x32 : not user-modifiable */ \
	0x02, /* 0x33 : not user-modifiable */ \
	0x08, /* 0x34 : not user-modifiable */ \
	0x00, /* 0x35 : not user-modifiable */ \
	0x08, /* 0x36 : not user-modifiable */ \
	0x10, /* 0x37 : not user-modifiable */ \
	0x01, /* 0x38 : not user-modifiable */ \
	0x01, /* 0x39 : not user-modifiable */ \
	0x00, /* 0x3a : not user-modifiable */ \
	0x00, /* 0x3b : not user-modifiable */ \
	0x00, /* 0x3c : not user-modifiable */ \
	0x00, /* 0x3d : not user-modifiable */ \
	0xff, /* 0x3e : not user-modifiable */ \
	0x00, /* 0x3f : not user-modifiable */ \
	0x0F, /* 0x40 : not user-modifiable */ \
	0x00, /* 0x41 : not user-modifiable */ \
	0x00, /* 0x42 : not user-modifiable */ \
	0x00, /* 0x43 : not user-modifiable */ \
	0x00, /* 0x44 : not user-modifiable */ \
	0x00, /* 0x45 : not user-modifiable */ \
	0x20, /* 0x46 : interrupt configuration 0->level low detection, 1-> level high, 2-> Out of window, 3->In window, 0x20-> New sample ready , TBC */ \
	0x0b, /* 0x47 : not user-modifiable */ \
	0x00, /* 0x48 : not user-modifiable */ \
	0x00, /* 0x49 : not user-modifiable */ \
	0x02, /* 0x4a : not user-modifiable */ \
	0x0a, /* 0x4b : not user-modifiable */ \
	0x21, /* 0x4c : not user-modifiable */ \
	0x00, /* 0x4d : not user-modifiable */ \
	0x00, /* 0x4e : not user-modifiable */ \
	0x05, /* 0x4f : not user-modifiable */ \
	0x00, /* 0x50 : not user-modifiable */ \
	0x00, /* 0x51 : not user-modifiable */ \
	0x00, /* 0x52 : not user-modifiable */ \
	0x00, /* 0x53 : not user-modifiable */ \
	0xc8, /* 0x54 : not user-modifiable */ \
	0x00, /* 0x55 : not user-modifiable */ \
	0x00, /* 0x56 : not user-modifiable */ \
	0x38, /* 0x57 : not user-modifiable */ \
	0xff, /* 0x58 : not user-modifiable */ \
	0x01, /* 0x59 : not user-modifiable */ \
	0x00, /* 0x5a : not user-modifiable */ \
	0x08, /* 0x5b : not user-modifiable */ \
	0x00, /* 0x5c : not user-modifiable */ \
	0x00, /* 0x5d : not user-modifiable */ \
	0x01, /* 0x5e : not user-modifiable */ \
	0xdb, /* 0x5f : not user-modifiable */ \
	0x0f, /* 0x60 : not user-modifiable */ \
	0x01, /* 0x61 : not user-modifiable */ \
	0xf1, /* 0x62 : not user-modifiable */ \
	0x0d, /* 0x63 : not user-modifiable */ \
	0x01, /* 0x64 : Sigma threshold MSB (mm in 14.2 format for MSB+LSB), use SetSigmaThreshold(), default value 90 mm  */ \
	0x68, /* 0x65 : Sigma threshold LSB */ \
	0x00, /* 0x66 : Min count Rate MSB (MCPS in 9.7 format for MSB+LSB), use SetSignalThreshold() */ \
	0x80, /* 0x67 : Min count Rate LSB */ \
	0x08, /* 0x68 : not user-modifiable */ \
	0xb8, /* 0x69 : not user-modifiable */ \
	0x00, /* 0x6a : not user-modifiable */ \
	0x00, /* 0x6b : not user-modifiable */ \
	0x00, /* 0x6c : Intermeasurement period MSB, 32 bits register, use SetIntermeasurementInMs() */ \
	0x00, /* 0x6d : Intermeasurement period */ \
	0x0f, /* 0x6e : Intermeasurement period */ \
	0x89, /* 0x6f : Intermeasurement period LSB */ \
	0x00, /* 0x70 : not user-modifiable */ \
	0x00, /* 0x71 : not user-modifiable */ \
	0x00, /* 0x72 : distance threshold high MSB (in mm, MSB+LSB), use SetD:tanceThreshold() */ \
	0x00, /* 0x73 : distance threshold high LSB */ \
	0x00, /* 0x74 : distance threshold low MSB ( in mm, MSB+LSB), use SetD:tanceThreshold() */ \
	0x00, /* 0x75 : distance threshold low LSB */ \
	0x00, /* 0x76 : not user-modifiable */ \
	0x01, /* 0x77 : not user-modifiable */ \
	0x0f, /* 0x78 : not user-modifiable */ \
	0x0d, /* 0x79 : not user-modifiable */ \
	0x0e, /* 0x7a : not user-modifiable */ \
	0x0e, /* 0x7b : not user-modifiable */ \
	0x00, /* 0x7c : not user-modifiable */ \
	0x00, /* 0x7d : not user-modifiable */ \
	0x02, /* 0x7e : not user-modifiable */ \
	0xc7, /* 0x7f : ROI center, use SetROI() */ \
	0xff, /* 0x80 : XY ROI (X=Width, Y=Height), use SetROI() */ \
	0x9B, /* 0x81 : not user-modifiable */ \
	0x00, /* 0x82 : not user-modifiable */ \
	0x00, /* 0x83 : not user-modifiable */ \
	0x00, /* 0x84 : not user-modifiable */ \
	0x01, /* 0x85 : not user-modifiable */ \
	0x00, /* 0x86 : clear interrupt, use ClearInterrupt() */ \
	0x00  /* 0x87 : start ranging, use StartRanging() or StopRanging(), If you want an automatic start after VL53L1X_init() call, put 0x40 in location 0x87 */ \
}


static inline int vl53l1x_init(struct vl53l1x *s) {
    static const uint8_t config[] = VL51L1X_DEFAULT_CONFIGURATION_INIT;
    for (uint32_t i=0; i<sizeof(config); i++) {
        vl53l1x_wr_u8(s, 0x2d+i, config[i]);
        if (s->status) goto error;
    }

    vl53l1x_start_ranging(s);
    if (s->status) goto error;

    /* We need to wait at least the default intermeasurement period of
       103ms before dataready will occur. But if a unit has already
       been powered and polling, it may happen much faster. */
    uint32_t dataReady = 0, timeout = 0;
    while (dataReady == 0) {
        dataReady = vl53l1x_check_for_data_ready(s); if (s->status) goto error;
        if (timeout++ > 150) { return VL53L1_ERROR_TIME_OUT; }
        VL51L1X_HAL_WAIT_MS(1);
    }
    vl53l1x_clear_interrupt(s);
    if (s->status) goto error;

    vl53l1x_stop_ranging(s);
    if (s->status) goto error;

    vl53l1x_wr_u8(s, VL53L1_VHV_CONFIG__TIMEOUT_MACROP_LOOP_BOUND, 0x09); /* two bounds VHV */
    if (s->status) goto error;

    vl53l1x_wr_u8(s, 0x0B, 0); /* start VHV from the previous temperature */
    if (s->status) goto error;

    /* Note: Original code does not perform status checks during
       init, and just passes the status value of the last
       command.  That can't be right!  Maybe add some checks here. */
    return 0;
error:
    // infof("status = %x\n", s->status);
    return s->status;
}



static inline int vl53l1x_begin(struct vl53l1x *s) {
    if (0xEACC != vl53l1x_get_sensor_id(s)) return 1;
    return vl53l1x_init(s);
}


/* For non-blocking operation, we provide abstraction around command
   generation for measurement commands.  These are exposed as array
   initializers that can then be used by a generic non-blocking i2c
   transfer mechanism.

   Actually, it's much simpler to just write the init wrappers and
   create a generic machine for WR_U8 and RD_U16.
*/
#define VL5311X_WR_U8(reg, val) { U16_BE(reg), val }
#define VL5311X_RD_U16(reg)     { U16_BE(reg) }

/* Write, Stop. */
#define VL5311X_CLEAR_INTERRUPT()      VL5311X_WR_U8(SYSTEM__MODE_START, 0x40)
#define VL5311X_STOP_RANGING()         VL5311X_WR_U8(SYSTEM__MODE_START, 0x00)
#define VL5311X_START_RANGING()        VL5311X_WR_U8(SYSTEM__MODE_START, 0x40)

/* Write, Stop, Read, Stop.  2 bytes big endian are returned. */
#define VL5311X_CHECK_FOR_DATA_READY() VL5311X_RD16(VL53L1_IDENTIFICATION__MODEL_ID)
#define VL5311X_GET_DISTANCE()         VL5311X_RD16(VL53L1_RESULT__FINAL_CROSSTALK_CORRECTED_RANGE_MM_SD)
#define VL5311X_GET_SENSOR_ID()        VL5311X_RD16(VL53L1_IDENTIFICATION__MODEL_ID)




/* For non-blocking operation, it is simpler to just use plain buffers
   and let upstream handle the send/receive.  This could do both
   polling and dma. */
static inline uint32_t vl53l1x_packet_wr_u8(uint8_t *buf, uint16_t reg, uint8_t val) {
    buf[0] = reg >> 8; buf[1] = reg;
    buf[2] = val;
    return 3;
}
static inline uint32_t vl53l1x_packet_wr_u16(uint8_t *buf, uint16_t reg, uint16_t val) {
    buf[0] = reg >> 8; buf[1] = reg;
    buf[2] = val >> 8; buf[3] = val;
    return 4;
}
static inline uint32_t vl53l1x_packet_rd_u16(uint8_t *buf, uint16_t reg) {
    buf[0] = reg >> 8; buf[1] = reg;
    return 2;
}
static inline uint32_t vl53l1x_packet_get_sensor_id(uint8_t *buf) {
    return vl53l1x_packet_rd_u16(buf, VL53L1_IDENTIFICATION__MODEL_ID);
}


#endif
