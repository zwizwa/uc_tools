#ifndef MOD_PDM_PWM_C
#define MOD_PDM_PWM_C

/* Pulse density modulator, v2.  Noise-shaped PWM.
   Same interface as mod_pdm.c, different implementation. */


#include "xorshift.h"
#include "pdm.h"


/* All setpoints assignments go through this function in case later it
   needs to be clipped.  The original circuit was not safe over the
   entire range so relied on this to be set carefully.

   Current hardware design does not have an unsafe setting.  For
   settings that are too high, a base resistor will limit current.
   This introduces a non-linearity that can be conmpensated for.

*/
#define PDM_SETPOINT_MIN 0x40000000ULL
#define PDM_SETPOINT_MAX 0xC0000000ULL
uint32_t pdm_safe_setpoint(uint32_t setpoint) {
#if 0
    if (setpoint < PDM_SETPOINT_MIN) return PDM_SETPOINT_MIN;
    if (setpoint > PDM_SETPOINT_MAX) return PDM_SETPOINT_MAX;
#endif
    return setpoint;
}



/* PLATFORM SPECIFIC */

/* CONFIGURATION */

#define PDM_CPU_USAGE_MARK GPIOA,3

/* This sets the number of channels.  Used for struct gen and code gen. */
#define PDM_FOR_CHANNELS(c) \
    c(0) c(1) c(2) c(3)


static inline void     control_trigger(void);

/* Don't use TIM2. It interacts badly with Flash programming. */
#define TIM_PDM      3
#define TIM_CONTROL  4


/* GPIOs corresponding to output compare channels. */
#define TIM3_GPIOS                              \
    {RCC_GPIOA, GPIOA, 6}, /* TIM_OC1 */        \
    {RCC_GPIOA, GPIOA, 7}, /* TIM_OC2 */        \
    {RCC_GPIOB, GPIOB, 0}, /* TIM_OC3 */        \
    {RCC_GPIOB, GPIOB, 1}  /* TIM_OC4 */

const struct hw_multi_pwm hw_pdm_config[] = {
//          rcc_tim   rcc_gpio   gpio           gpio_config,               div       duty        irq (optional)
//---------------------------------------------------------------------------------------------------------------
    [3] = { RCC_TIM3, TIM3,      {TIM3_GPIOS},  HW_GPIO_CONFIG_ALTFN_2MHZ, PDM_DIV,  PDM_DIV/2,  NVIC_TIM3_IRQ },
};
#define C_PDM hw_pdm_config[TIM_PDM]

void pdm_start(void) {
    infof("start\n");
    hw_multi_pwm_init(C_PDM);
    hw_multi_pwm_start(C_PDM);
}
void pdm_stop(void) {
    hw_multi_pwm_stop(C_PDM);
}

#define CONTROL_DIV 256
uint32_t control_div_count = 0;

struct channel {
    uint32_t setpoint;
    struct pdm3 pdm;
};
#define CHANNEL_STRUCT(c) {},
struct channel pdm_channel[] = { PDM_FOR_CHANNELS(CHANNEL_STRUCT) };

#define PDM_NB_CHANNELS ARRAY_SIZE(pdm_channel)

static inline void pdm_update_channel(int i) {
    struct channel *c = &pdm_channel[i];
    uint32_t val = pdm3_update(&c->pdm, c->setpoint, 32 - PDM_DIV_LOG);
    hw_multi_pwm_duty(C_PDM, i, val);
}
#define PDM_UPDATE_CHANNEL(c) pdm_update_channel(c);

/* PDM TIMER INTERRUPT */


void HW_TIM_ISR(TIM_PDM)(void) {
    hw_multi_pwm_ack(C_PDM);
    hw_gpio_high(PDM_CPU_USAGE_MARK);

#if 0
    PDM_FOR_CHANNELS(PDM_UPDATE_CHANNEL)

#else
    hw_multi_pwm_duty(
        C_PDM, 0,
        pdm3_update(&pdm_channel[0].pdm,
                    pdm_channel[0].setpoint,
                    32 - PDM_DIV_LOG));

    hw_multi_pwm_duty(
        C_PDM, 1,
        pdm3_update(&pdm_channel[1].pdm,
                    pdm_channel[1].setpoint,
                    32 - PDM_DIV_LOG));

    hw_multi_pwm_duty(
        C_PDM, 2,
        pdm3_update(&pdm_channel[2].pdm,
                    pdm_channel[2].setpoint,
                    32 - PDM_DIV_LOG));

    hw_multi_pwm_duty(
        C_PDM, 3,
        pdm3_update(&pdm_channel[3].pdm,
                    pdm_channel[3].setpoint,
                    32 - PDM_DIV_LOG));
#endif

    if (control_div_count == 0) {
        /* Swap buffers: previously computed control values are now
           used in main PDM/PWM interrupt, and the control interrupt
           can start a new update. */
        // FIXME
        control_trigger();
    }
    control_div_count = (control_div_count + 1) % CONTROL_DIV;
    hw_gpio_low(PDM_CPU_USAGE_MARK);
}



void pdm_init(void) {
    /* PDM frame clock.  This is set/reset at beginning/end of PDM
       ISR, so can be used as a CPU usage measurement. */
    hw_gpio_config(
        PDM_CPU_USAGE_MARK,
        HW_GPIO_CONFIG_OUTPUT);

    /* Move all setpoints into a safe range before starting the
       modulator. */
    for(int i=0; i<PDM_NB_CHANNELS; i++) {
        pdm_channel[i].setpoint = pdm_safe_setpoint(0x40000000ULL);
    }
    pdm_channel[0].setpoint = 2000000000;

}





#endif
