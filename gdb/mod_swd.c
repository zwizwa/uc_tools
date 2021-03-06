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
   https://research.kudelskisecurity.com/2019/07/31/swd-part-2-the-mem-ap/

   SWDIO data is set by host during rising edge, and sampled by DP during falling edge of SWCLK

   Each SWD transaction has three phases:
   - Request phase. 8 bits ->DP
   - Ack phase,     3 bits <-DP
   - Data phase,    up to 32 bits <->DP, with odd parity bit
   - Turnaround cycle for every direction switch
   - 46 clocks total
   - at least 1 idle (0) cycle between transactions

   DONE:
   - Basic read transaction is working.  Can read IDCODE
   TODO:
   - Write transaction
   - Other registers, indirections


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

#define SWD_FLAGS_PARITY (1<<0)

struct swd_cmd {
    void *next;
    uint32_t arg;
    uint32_t dr;
    int8_t i;
    int8_t flags;
    int8_t cmd;
    int8_t res;
};
void swd_cmd_init(struct swd_cmd *s, uint8_t cmd, uint32_t arg) {
    ZERO(s);
    s->cmd = cmd;
    s->arg = arg;
}

#define SWD_DELAY(s) hw_busywait(1) // FIXME

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

#if 0
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
#endif


#define SWD_WRITE_LSB(s, bits_vec, bits_nb) {            \
        SWD_INFO_BIT("w:");                              \
        s->dr = bits_vec;                                \
        for (s->i = 0; s->i < (bits_nb); s->i++) {       \
            int bit = (s->dr >> s->i) & 1;               \
            if (bit) {                                   \
                s->flags ^= SWD_FLAGS_PARITY;            \
            }                                            \
            SWD_INFO_BIT("%d", bit);                     \
            SWD_WRITE_BIT(s, bit);                       \
        }                                                \
        SWD_INFO_BIT("\n");                              \
    }

#define SWD_READ_LSB(s, bits_nb) ({                           \
            SWD_INFO_BIT("r:");                               \
            s->dr = 0;                                        \
            for (s->i = 0; s->i < (bits_nb); s->i++) {        \
                int bit = SWD_READ_BIT(s) & 1;                \
                SWD_INFO_BIT("%d", bit);                      \
                if (bit) {                                    \
                    s->dr |= (1 << s->i);                     \
                    s->flags ^= SWD_FLAGS_PARITY;             \
                };                                            \
            }                                                 \
            SWD_INFO_BIT("\n");                               \
            s->dr;                                            \
        })
#define SWD_RESET(s,n1,n0) {                                            \
        for (s->i = 0; s->i<n1; s->i++) { SWD_WRITE_BIT(s, 1); }        \
        for (s->i = 0; s->i<n0; s->i++) { SWD_WRITE_BIT(s, 0); }        \
    }


uint32_t swd_cmd_hdr(uint32_t port, uint32_t read, uint32_t addr) {
    uint32_t hdr =
        (1<<0) /* start */ |
        ((1 & port)<<1) |
        ((1 & read)<<2) |
        ((0b1100 & addr)<<1) |
        ((1 & (port ^ read ^ (addr >> 2) ^ (addr >> 3))) << 5) /* parity */ |
        (1<<7) /* park */;
    return hdr;
}

// Select Debug Port or Access Port
#define SWD_PORT_DP 0
#define SWD_PORT_AP 1

// Read or write transaction
#define SWD_READ 1
#define SWD_WRITE 0

// The A[3:2] address bits, using same 0,4,8,C numbering as manual.
#define SWD_DP_RD_DPIDR    0
#define SWD_DP_RD_CTRLSTAT 0x4
#define SWD_DP_WR_CTRLSTAT 0x4
#define SWD_DP_WR_SELECT   0x8
#define SWD_DP_RD_RDBUFF   0xc

#define SWD_CTRLSTAT_CDBGRSTREQ (1 << 26)
#define SWD_CTRLSTAT_CDBGRSTACK (1 << 27)
#define SWD_CTRLSTAT_CDBGPWRUPREQ (1 << 28)
#define SWD_CTRLSTAT_CDBGPWRUPACK (1 << 29)
#define SWD_CTRLSTAT_CSYSPWRUPREQ (1 << 30)
#define SWD_CTRLSTAT_CSYSPWRUPACK (1 << 31)


/* Register documentation is in
   ARM Debug Interface Architecture Specification ADIv5.0 to ADIv5.2
   IHI0031C_debug_interface_as.pdf

   Section 2.3 DP register descriptions

*/

sm_status_t swd_cmd_tick(struct swd_cmd *s) {
    SM_RESUME(s);
    //infof("swd_cmd start %x\n", s->cmd);
    switch(s->cmd) {
    case 0:
        /* Initialize.  The number of low bits after the 50 x high
           reset sequence is significant.  The first has to be 0x, the
           second has to be 2x. */
        swd_dir(SWD_OUT);
        SWD_RESET(s,50,0);
        //SWD_WRITE_MSB(s, 0b0111100111100111, 16);
        SWD_WRITE_LSB(s, 0xE79E, 16);
        SWD_RESET(s,50,2);
        /* Perform READID
           1  start bit
           0  debug port
           1  read
           00 DPIDR register
           1  parity
           0  stop
           1  park */
        s->cmd = swd_cmd_hdr(SWD_PORT_DP, SWD_READ, SWD_DP_RD_DPIDR);
        /* FALLTHROUGH */

    default:
        /* Transaction. */
        swd_dir(SWD_OUT);
        //SWD_WRITE_LSB(s, 0b10100101, 8);
        SWD_WRITE_LSB(s, s->cmd, 8);
        swd_dir(SWD_IN);
 
        /* FIXME: shouldn't there be a TRN bit here? */

        s->res = SWD_READ_LSB(s, 3);
        if (0b001 != s->res) {
            /* not ACK */
            infof("res = %d\n", s->res);
        }
        s->flags &= ~SWD_FLAGS_PARITY; // reset
        if (s->cmd & (1 << 2)) {
            /* Read */
            SWD_READ_LSB(s, 32); // sets s->dr
            int parity = SWD_READ_BIT(s);
            SWD_INFO_BIT("p:%d\n", parity);
            if (parity) s->flags ^= SWD_FLAGS_PARITY;
            SWD_READ_BIT(s); // turn
            // s->dr still contains value
        }
        else {
            /* Write */
            SWD_READ_BIT(s); // turn
            SWD_READ_BIT(s); // turn
            swd_dir(SWD_OUT);
            SWD_WRITE_LSB(s, s->arg, 32);
            int parity = !!(s->flags & SWD_FLAGS_PARITY);
            SWD_INFO_BIT("p:%d\n", parity);
            SWD_WRITE_BIT(s, parity);;
        }
        SWD_WRITE_BIT(s, 0); // idle
        break;
    }

    swd_dir(SWD_IN);
    //infof("swd_cmd halt\n");
    SM_HALT(s);
}

/* High level command server. */
struct swd_serv {
    void *next;
    union {
        struct swd_cmd swd_cmd;
    } sub;
};
void swd_serv_init(struct swd_serv *s) {
    ZERO(s);
}

#define SWD_DP_READ(s, reg)                             \
    SM_SUB_CATCH(                                       \
        s, swd_cmd,                                     \
        swd_cmd_hdr(SWD_PORT_DP, SWD_READ, reg), 0)

#define SWD_AP_READ(s, reg)                             \
    SM_SUB_CATCH(                                       \
        s, swd_cmd,                                     \
        swd_cmd_hdr(SWD_PORT_AP, SWD_READ, reg), 0)

#define SWD_DP_WRITE(s, reg, val)                       \
    SM_SUB_CATCH(                                       \
        s, swd_cmd,                                     \
        swd_cmd_hdr(SWD_PORT_DP, SWD_WRITE, reg), val)


sm_status_t swd_serv_tick(struct swd_serv *s) {
    struct swd_cmd *c = &s->sub.swd_cmd;
    SM_RESUME(s);
    /* Initialize SWD. */
    if (SM_SUB_CATCH(s, swd_cmd, 0, 0)) goto error;
    // stm32f103 is 1ba01477
    infof("dpidr 0x%08x, p=%d\n", c->dr, !!(c->flags & SWD_FLAGS_PARITY));

    /* Read dpidr again. */
    if (SWD_DP_READ(s, SWD_DP_RD_DPIDR)) goto error;
    infof("dpidr 0x%08x, p=%d\n", c->dr, !!(c->flags & SWD_FLAGS_PARITY));

    /* Read status register. */
    if (SWD_DP_READ(s, SWD_DP_RD_CTRLSTAT)) goto error;
    infof("ctrlstat 0x%08x, p=%d\n", c->dr, !!(c->flags & SWD_FLAGS_PARITY));

    /* Power up debug domain.*/
    if (SWD_DP_WRITE(s, SWD_DP_WR_CTRLSTAT,
                     0
                     |SWD_CTRLSTAT_CDBGPWRUPREQ
                     //|SWD_CTRLSTAT_CDBGPWRUPACK
                     |SWD_CTRLSTAT_CSYSPWRUPREQ
                     //|SWD_CTRLSTAT_CSYSPWRUPACK
           )) goto error;

    hw_busywait_ms(10);

    /* Read status register. */
    if (SWD_DP_READ(s, SWD_DP_RD_CTRLSTAT)) goto error;
    infof("ctrlstat 0x%08x, p=%d\n", c->dr, !!(c->flags & SWD_FLAGS_PARITY));

    /* Read 0xFC (bank 0xF, reg 0xC) in AP 0 */
    if (SWD_DP_WRITE(s, SWD_DP_WR_SELECT,
                      (0xf << 4)  /* APBANKSEL */
                     |(0   << 0)  /* DPBANKSEL */
                     |(0   << 24) /* APSEL */
            )) goto error;
    if (SWD_AP_READ(s, 0xC)) goto error;
    if (SWD_DP_READ(s, SWD_DP_RD_RDBUFF)) goto error;


#if 0
    /* Read dpidr again. */
    if (SWD_DP_READ(s, SWD_DP_RD_DPIDR)) goto error;
    infof("dpidr 0x%08x, p=%d\n", c->dr, !!(c->flags & SWD_FLAGS_PARITY));

    /* Read status register. */
    if (SWD_DP_READ(s, SWD_DP_RD_CTRLSTAT)) goto error;
    infof("ctrlstat 0x%08x, p=%d\n", c->dr, !!(c->flags & SWD_FLAGS_PARITY));
#endif

    SM_HALT(s);
  error:
    infof("error\n");
    SM_HALT_STATUS(s, 1);
}


DEF_INSTANCE(swd);

#endif
