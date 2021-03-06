#ifndef MOD_SWD
#define MOD_SWD

/* To hell with off the shelf debugging adaptors.
   What can I do with a bluepill?

   https://developer.arm.com/architectures/cpu-architecture/debug-visibility-and-trace/coresight-architecture/serial-wire-debug
   https://en.wikipedia.org/wiki/JTAG#Similar_interface_standards
   https://research.kudelskisecurity.com/2019/05/16/swd-arms-alternative-to-jtag
   https://www.cnblogs.com/shangdawei/p/4748751.html
   https://developer.arm.com/documentation/ddi0316/d/functional-description/swj-dp/swd-and-jtag-selection-mechanism

   https://static.docs.arm.com/ihi0031/c/IHI0031C_debug_interface_as.pdf

   SWDIO data is set by host during rising edge, and sampled by DP during falling edge of SWCLK

   Each SWD transaction has three phases:
   - Request phase. 8 bits ->DP
   - Ack phase,     3 bits <-DP
   - Data phase,   up to 32 bits <->DP, with odd parity bit


*/

#include "instance.h"
#include "sm.h"

#define SWD_GPIO_SWDIO GPIOA,0
#define SWD_GPIO_SWCLK GPIOA,1
#define SWD_GPIO_RESET GPIOA,2

#define SWD_OUT 0
#define SWD_IN  1

INLINE int  swd_get_swdio(void)     { return hw_gpio_read(SWD_GPIO_SWDIO); }
INLINE void swd_set_swdio(int val)  { hw_gpio_write(SWD_GPIO_SWDIO, val); }
INLINE void swd_swclk(int val)      { hw_gpio_write(SWD_GPIO_SWCLK, val); }
INLINE void swd_reset(int val)      { hw_gpio_write(SWD_GPIO_RESET, val); }
INLINE void swd_dir(int in) {
    hw_gpio_config(
        SWD_GPIO_SWDIO, in ?
        HW_GPIO_CONFIG_INPUT :
        HW_GPIO_CONFIG_OUTPUT);
}

instance_status_t swd_init(instance_init_t *ctx) {
    int clk = hw_gpio_read(SWD_GPIO_SWCLK);
    int dio = hw_gpio_read(SWD_GPIO_SWDIO);
    infof("swd_init clk=%d, dio=%d\n", clk, dio);

    swd_swclk(0); hw_gpio_config(SWD_GPIO_SWCLK, HW_GPIO_CONFIG_OUTPUT);
    swd_reset(1); hw_gpio_config(SWD_GPIO_RESET, HW_GPIO_CONFIG_OUTPUT);
    swd_set_swdio(1);
    swd_dir(SWD_OUT);
    return 0;
}

struct swd_task {
    void *next;
    uint32_t dr;
    int32_t i;  // signed, so it can contain -1 as a stop condition
    uint32_t cmd;
};
void swd_task_init(struct swd_task *s, uint32_t cmd) {
    ZERO(s);
    s->cmd = cmd;
}

#define SWD_DELAY(s) hw_busywait(10) // FIXME

/* Precondition for these two macros: clock is low, and delay still
   needed before setting clock high.  Direction needs to be set
   elsewhere. */
#define SWD_WRITE_BIT(s, bit) {                 \
            swd_set_swdio(bit);                 \
            SWD_DELAY(s);                       \
            swd_swclk(1);                       \
            SWD_DELAY(s);                       \
            swd_swclk(0);                       \
    }
#define SWD_READ_BIT(s) ({                      \
            SWD_DELAY(s);                       \
            swd_swclk(1);                       \
            SWD_DELAY(s);                       \
            swd_swclk(0);                       \
            swd_get_swdio();                    \
        })

#define SWD_INFO_BIT infof

#define SWD_WRITE_MSB(s, bits_vec, bits_nb) {            \
        SWD_INFO_BIT("w:");                              \
        s->dr = bits_vec;                                \
        for (s->i = (bits_nb)-1; s->i >= 0; s->i--) {    \
            int bit = (s->dr >> s->i) & 1;               \
            SWD_INFO_BIT("%d", bit);                     \
            SWD_WRITE_BIT(s, bit);                       \
        }                                                \
        SWD_INFO_BIT("\n");                              \
    }
#define SWD_READ_MSB(s, bits_nb) ({                           \
            SWD_INFO_BIT("r:");                               \
            s->dr = 0;                                        \
            for (s->i = 0; s->i < (bits_nb); s->i++) {        \
                int bit = SWD_READ_BIT(s) & 1;                \
                SWD_INFO_BIT("%d", bit);                      \
                s->dr = bit | (s->dr << 1);                   \
            }                                                 \
            SWD_INFO_BIT("\n");                               \
            s->dr;                                            \
        })
#define SWD_RESET(s,n1,n0) {                                            \
        for (s->i = 0; s->i<n1; s->i++) { SWD_WRITE_BIT(s, 1); }        \
        for (s->i = 0; s->i<n0; s->i++) { SWD_WRITE_BIT(s, 0); }        \
    }


sm_status_t swd_task_tick(struct swd_task *s) {
    SM_RESUME(s);
    infof("swd_task start %x\n", s->cmd);
    if (!s->cmd) goto halt;
    /* Initialize.  The number of zero bits after the 50 x 1 reset
       sequence is significant.  The first has to be 0x, the second
       has to be 2x. */
    swd_dir(SWD_OUT);
    SWD_RESET(s,50,0);
    SWD_WRITE_MSB(s, 0b0111100111100111, 16);
    SWD_RESET(s,50,2);

    // Perform READID
    // 1  start bit
    // 0  debug port
    // 1  read
    // 00 IDCODE register
    // 1  parity
    // 0  stop
    // 1  park
    //                 10100101
    SWD_WRITE_MSB(s, 0b10100101, 8);

    swd_dir(SWD_IN);

    //swd_dir(SWD_OUT); swd_set_swdio(0);
    SWD_READ_MSB(s, 32);
  halt:
    swd_dir(SWD_IN);
    infof("swd_task halt\n");
    SM_HALT(s);
}

DEF_INSTANCE(swd);

#endif
