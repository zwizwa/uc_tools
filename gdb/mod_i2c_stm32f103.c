#ifndef MOD_I2C_STM32F103
#define MOD_I2C_STM32F103

#ifndef EMU


/* 1. CONFIGURATION */

#ifndef I2C_PIN_SCL
#define I2C_PIN_SCL 8
#endif

#ifndef I2C_PIN_SDA
#define I2C_PIN_SDA 9
#endif

#ifndef I2C_GPIO
#define I2C_GPIO GPIOB
#endif

/* 2. HAL */

// FIXME: Just a stub to make emulator compile. Move this to the proper place

#include "hw_stm32f103.h"
#include "cycle_counter.h"


/* This is still tied to stm32f103, but there's no reason why it can't
   be made portable. */

/* On STM32, pull direction is set by the output data register ODR,
   which we access through hw_gpio_write().  There are only 2 states.
   These high()/low() functions provide glitch-free switching between
   them.  Order of operations is important.  Annotated with Hoare
   logic conditions. */
static inline void i2c_pin_high(uint32_t gpio, uint32_t pin) {
    // PRE: if pulling high, no state changes.
    // PRE: if asserting low, state changes annotated below
    hw_gpio_config(gpio, pin, HW_GPIO_CONFIG_INPUT_PULL);
    // pulling low
    hw_gpio_write(gpio, pin, 1);
    // POST: pulling high
}

static inline void i2c_pin_low(uint32_t gpio, uint32_t pin) {
    // PRE: if asserting low, no state changes
    // PRE: if apulling high, state changes annotated below
    hw_gpio_write(gpio, pin, 0);
    // pulling low
    hw_gpio_config(gpio, pin, HW_GPIO_CONFIG_OUTPUT);
    // POST: asserting low
}
static inline void i2c_pin_set(uint32_t gpio, uint32_t pin, uint32_t val) {
    if (val)
        i2c_pin_high(gpio, pin);
    else
        i2c_pin_low(gpio, pin);
}

/* No context needed: calls are bound directly to GPIO registers. */

/* Note that "bus" and "port" names are used in a sloppy way, but
   typically a bus is a connection of ports.  The distinction is clear
   in emulation: the bus state is the logic level determined from the
   state of all the ports, while the port state is determined by
   pulldown or high-Z for clock and data. */
struct i2c_port {
};

static inline void i2c_write_sda(struct i2c_port *s, int bitval) {
    i2c_pin_set(I2C_GPIO, I2C_PIN_SDA, bitval);
}
static inline void i2c_write_scl(struct i2c_port *s, int bitval) {
    i2c_pin_set(I2C_GPIO, I2C_PIN_SCL, bitval);
}
static inline int i2c_read_sda(struct i2c_port *s) {
    return hw_gpio_read(I2C_GPIO, I2C_PIN_SDA);
}
static inline int i2c_read_scl(struct i2c_port *s) {
    return hw_gpio_read(I2C_GPIO, I2C_PIN_SCL);
}

#endif



#endif
