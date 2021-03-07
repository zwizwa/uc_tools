#ifndef MOD_SWD
#define MOD_SWD

/* To hell with off the shelf debugging adaptors.
   What can I do with a bluepill?

   See documentation below.

   TL&DR: There are a couple of layers to the protocol.

   Specific to STM32F103, from high level down to low level:

   1. You probably want to only perform MEM-AP access, to read out
      words from the processor's memory space.  This can be done while
      it is running.

   2. Such a memory access uses the CSW (config), TAR (target
      address), and DRW (data read/write) registers in the MEM-AP: set
      TAR, and read DRW.

   3. Each of the individual reads and writes to those registers
      requires two to three SWD transactions: (optionally) select
      bank, perform AP read or write, and perform DP read for result
      or status.

   4. A single SWD transaction consists of a packet of 46 bits, spaced
      by at least one idle period (0).

      - Request phase. 8 bits ->DP
      - Ack phase,     3 bits <-DP
      - Data phase,    up to 32 bits <->DP, with odd parity bit
      - Turnaround cycle for every direction switch
      - 46 clocks total
      - at least 1 idle (0) cycle between transactions



   https://developer.arm.com/architectures/cpu-architecture/debug-visibility-and-trace/coresight-architecture/serial-wire-debug
   https://en.wikipedia.org/wiki/JTAG#Similar_interface_standards
   https://research.kudelskisecurity.com/2019/05/16/swd-arms-alternative-to-jtag
   https://www.cnblogs.com/shangdawei/p/4748751.html
   https://developer.arm.com/documentation/ddi0316/d/functional-description/swj-dp/swd-and-jtag-selection-mechanism

   https://static.docs.arm.com/ihi0031/c/IHI0031C_debug_interface_as.pdf
   https://research.kudelskisecurity.com/2019/07/31/swd-part-2-the-mem-ap/
   https://developer.arm.com/documentation/ddi0439/b/Debug/About-the-AHB-AP/AHB-AP-programmers-model

   https://developer.arm.com/documentation/100230/0002/appendices/debug-access-port/dap-register-descriptions/ahb-ap-register-descriptions


   SWDIO data is set by host during rising edge, and sampled by DP during falling edge of SWCLK


   This was originally written as a SM, but that turned out to be too
   complicated, and since bitbang seems to work without any busy
   delays, it seems not worth it.  So I'm factoring out some bits into
   a header file to be used in both implementations.

*/

#include "instance.h"
#include "sm.h"

#define SWD_GPIO_SWDIO GPIOA,0
#define SWD_GPIO_SWCLK GPIOA,1
#define SWD_GPIO_RESET GPIOA,2

#define SWD_OUT 0
#define SWD_IN  1

INLINE int  swd_get_swdio(void)     { return hw_gpio_read(SWD_GPIO_SWDIO); }
INLINE void swd_set_swdio(int val)  { hw_gpio_write_v2(SWD_GPIO_SWDIO, val); }
INLINE void swd_swclk(int val)      { hw_gpio_write_v2(SWD_GPIO_SWCLK, val); }
INLINE void swd_reset(int val)      { hw_gpio_write_v2(SWD_GPIO_RESET, val); }
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
    uint32_t val;
    uint32_t dr;
    int8_t i;
    int8_t flags;
    int8_t cmd;
    int8_t res;
};
void swd_cmd_init(struct swd_cmd *s, uint8_t cmd, uint32_t val) {
    ZERO(s);
    s->cmd = cmd;
    s->val = val;
}

#define SWD_DELAY(s) //hw_busywait(1) // FIXME

/* Precondition for these two macros: clock is low, and delay still
   needed before setting clock high.  Direction needs to be set
   elsewhere. */

//                              ___________
//                       ______|   w       |___r___
#define SWD_W(d, c, w)  { d; c(1); w; d; c(0);    }
#define SWD_R(d, c, r) ({ d; c(1);    d; c(0); r; })

#define SWD_WRITE_BIT(s, bit) SWD_W(SWD_DELAY(s), swd_swclk, swd_set_swdio(bit))
#define SWD_READ_BIT(s)       SWD_R(SWD_DELAY(s), swd_swclk, swd_get_swdio())


//#define SWD_INFO_BIT infof
#define SWD_INFO_BIT(...)

#define SWD_WRITE_LSB(s, bits_vec, bits_nb) \
        SWD_WRITE_LSB_(s, bits_vec, bits_nb, s->dr, s->i, s->flags)

#define SWD_WRITE_LSB_(s, bits_vec, bits_nb, dr, i, flags) {    \
        SWD_INFO_BIT("w:");                                     \
        dr = bits_vec;                                          \
        for (i = 0; i < (bits_nb); i++) {                       \
            int bit = dr & 1;                                   \
            if (bit) {                                          \
                flags ^= SWD_FLAGS_PARITY;                      \
            }                                                   \
            SWD_INFO_BIT("%d", bit);                            \
            SWD_WRITE_BIT(s, bit);                              \
            dr >>= 1;                                           \
        }                                                       \
        SWD_INFO_BIT("\n");                                     \
    }
#define SWD_READ_LSB(s, bits_nb) \
    SWD_READ_LSB_(s, bits_nb, s->dr, s->i, s->flags)

#define SWD_READ_LSB_(s, bits_nb, dr, i, flags) ({            \
            SWD_INFO_BIT("r:");                               \
            dr = 0;                                           \
            for (i = 0; i < (bits_nb); i++) {                 \
                int bit = SWD_READ_BIT(s) & 1;                \
                SWD_INFO_BIT("%d", bit);                      \
                dr >>= 1;                                     \
                if (bit) {                                    \
                    dr |= (1 << 31);                          \
                    flags ^= SWD_FLAGS_PARITY;                \
                };                                            \
            }                                                 \
            SWD_INFO_BIT("\n");                               \
            dr >>= (32-bits_nb);                              \
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

#define SWD_MEM_AP_CSW 0x0
#define SWD_MEM_AP_TAR 0x4
#define SWD_MEM_AP_DRW 0xC


/* Register documentation is in
   ARM Debug Interface Architecture Specification ADIv5.0 to ADIv5.2
   IHI0031C_debug_interface_as.pdf

   Section 2.3 DP register descriptions

*/

/* These are factored out as macros to allow SM and blocking inlined
   implementation.  __always_inline__ does not work with computed
   goto. */

/* Initialize.  The number of low bits after the 50 x high reset
   sequence is significant.  The first has to be 0x, the second has to
   be 2x. */
#define SWD_INITIALIZE(s) {                             \
        swd_dir(SWD_OUT);                               \
        SWD_RESET(s,50,0);                              \
        SWD_WRITE_LSB(s, 0xE79E, 16);                   \
        SWD_RESET(s,50,2);                              \
    }

/* Transaction based on the _LSB macros.  Note that read and write
   cycles are both 46 clocks, but the location of the turn phases is
   different.  At least one idle cycle is necessary at the end. */

/* The core routine is factored out in terms of direct members.  GCC
   doesn't seem to be smart enough to (or doesn't have enough
   information such that it can) eliminate the state struct.  The only
   references to s that are left are only necessary when this is
   compiled to SM. */
#define SWD_TRANSACTION(s) \
    SWD_TRANSACTION_(s,s->cmd,s->res,s->flags,s->val,s->dr,s->i)

#define SWD_TRANSACTION_(s,cmd,res,flags,val,dr,i) {            \
        flags = 0;                                              \
        dr = 0;                                                 \
        swd_dir(SWD_OUT);                                       \
        SWD_WRITE_LSB_(s, cmd, 8, dr, i, flags);                \
        swd_dir(SWD_IN);                                        \
        SWD_READ_BIT(s); /* turn */                             \
        res = SWD_READ_LSB_(s, 3, dr, i, flags);                \
        if (0b001 != res) {                                     \
            /* not ACK */                                       \
            LOG("res = %d\n", res);                             \
        }                                                       \
        flags &= ~SWD_FLAGS_PARITY; /* reset */                 \
        if (cmd & (1 << 2)) {                                   \
            /* Read */                                          \
            SWD_READ_LSB_(s, 32, dr, i, flags);                 \
            val = dr;                                           \
            int parity = SWD_READ_BIT(s);                       \
            SWD_INFO_BIT("p:%d\n", parity);                     \
            if (parity) flags ^= SWD_FLAGS_PARITY;              \
            SWD_READ_BIT(s); /* turn */                         \
        }                                                       \
        else {                                                  \
            /* Write */                                         \
            SWD_READ_BIT(s); /* turn */                         \
            swd_dir(SWD_OUT);                                   \
            SWD_WRITE_LSB_(s, val, 32, dr, i, flags);           \
            int parity = !!(flags & SWD_FLAGS_PARITY);          \
            SWD_INFO_BIT("p:%d\n", parity);                     \
            SWD_WRITE_BIT(s, parity);;                          \
        }                                                       \
        SWD_WRITE_BIT(s, 0); /* idle */                         \
        swd_dir(SWD_IN);                                        \
    }

static inline sm_status_t swd_cmd_tick(struct swd_cmd *s) {
    SM_RESUME(s);
    //infof("swd_cmd start %x\n", s->cmd);
    switch(s->cmd) {
    case 0:
        SWD_INITIALIZE(s)
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
        SWD_TRANSACTION(s);
        break;
    }

    //infof("swd_cmd halt\n");
    SM_HALT(s);
}

#if 1
//#define SWD_KEEP KEEP  // just for looking at assembly
#define SWD_KEEP // don't keep at link time if not used, but still compile
/* Optimized blocking versions.  Note that a single transaction is
   fast enough, so it makes sense to use these as atomics in a
   cooperative multitasking environment.  However, speed isn't an
   issue yet, so these are not used. */
SWD_KEEP void swd_transaction(uint32_t port, uint32_t read, uint32_t reg) {
    uint32_t res = 0, flags = 0, val = 0, dr = 0, i = 0;
    uint32_t cmd = swd_cmd_hdr(port, read, reg);
    SWD_TRANSACTION_(NULL,cmd,res,flags,val,dr,i);
}
void swd_log_flags(uint32_t flags) {
    if (flags) {
        // FIXME: need error mechanism
        infof("flags = 0x%0x\n", flags);
    }
}
SWD_KEEP void swd_write_lsb(uint32_t bits_vec, uint32_t bits_nb) {
    uint32_t dr=0, i=0, flags=0;
    SWD_WRITE_LSB_(NULL, bits_vec, bits_nb, dr, i, flags);
    swd_log_flags(flags);
}
SWD_KEEP uint32_t swd_read_lsb(uint32_t bits_nb) {
    uint32_t dr=0, i=0, flags=0;
    SWD_READ_LSB_(NULL, bits_nb, dr, i, flags);
    swd_log_flags(flags);
    return dr;
}
#endif


/* High level command server. */
struct swd_serv {
    void *next;
    int8_t i,j,k,l;
    union {
        struct swd_cmd swd_cmd;
    } sub;
};
void swd_serv_init(struct swd_serv *s) {
    ZERO(s);
}

/* Single transactions. */
#define SWD_TRANS_DP_READ(s, reg)                       \
    SM_SUB_CATCH(                                       \
        s, swd_cmd,                                     \
        swd_cmd_hdr(SWD_PORT_DP, SWD_READ, reg), 0)

#define SWD_TRANS_AP_READ(s, reg)                       \
    SM_SUB_CATCH(                                       \
        s, swd_cmd,                                     \
        swd_cmd_hdr(SWD_PORT_AP, SWD_READ, reg), 0)

#define SWD_TRANS_DP_WRITE(s, reg, val)                 \
    SM_SUB_CATCH(                                       \
        s, swd_cmd,                                     \
        swd_cmd_hdr(SWD_PORT_DP, SWD_WRITE, reg), val)

#define SWD_TRANS_AP_WRITE(s, reg, val)                 \
    SM_SUB_CATCH(                                       \
        s, swd_cmd,                                     \
        swd_cmd_hdr(SWD_PORT_AP, SWD_WRITE, reg), val)

/* Read/Write AP registers requires two transactions. Note that these
   do not change banks.  For MEM-AP that is not necessary. */
#define SWD_AP_REG_READ(s, reg) {                                     \
        if (SWD_TRANS_AP_READ(s, reg & 0b1100)) goto error;           \
        if (SWD_TRANS_DP_READ(s, SWD_DP_RD_RDBUFF)) goto error;       \
    }
#define SWD_AP_REG_WRITE(s, reg, val) {                                 \
        if (SWD_TRANS_AP_WRITE(s, reg & 0b1100, val)) goto error;       \
        /* Read status register, otherwise subsequent AP_READ returns WAIT? */ \
        if (SWD_TRANS_DP_READ(s, SWD_DP_RD_CTRLSTAT)) goto error;       \
    }

/* MEM-AP access uses another layer of indirection: set TAR, read DRW */
#define SWD_MEM_AP_READ(s, addr) {                              \
        SWD_AP_REG_WRITE(s, SWD_MEM_AP_TAR, addr);              \
        SWD_AP_REG_READ(s, SWD_MEM_AP_DRW);                     \
    }

#define SWD_MEM_AP_WRITE(s, addr, val) {                        \
        SWD_AP_REG_WRITE(s, SWD_MEM_AP_TAR, addr);              \
        SWD_AP_REG_WRITE(s, SWD_MEM_AP_DRW, val);               \
    }


sm_status_t swd_serv_tick(struct swd_serv *s) {
    struct swd_cmd *c = &s->sub.swd_cmd;
    SM_RESUME(s);
    /* Initialize SWD. */
    if (SM_SUB_CATCH(s, swd_cmd, 0, 0)) goto error;
    // stm32f103 is 1ba01477
    infof("dpidr 0x%08x, p=%d\n", c->val, !!(c->flags & SWD_FLAGS_PARITY));

    /* Read status register. */
    if (SWD_TRANS_DP_READ(s, SWD_DP_RD_CTRLSTAT)) goto error;
    infof("ctrlstat 0x%08x, p=%d\n", c->val, !!(c->flags & SWD_FLAGS_PARITY));

    /* Power up debug domain.*/
    if (SWD_TRANS_DP_WRITE(s, SWD_DP_WR_CTRLSTAT,
                     SWD_CTRLSTAT_CDBGPWRUPREQ | SWD_CTRLSTAT_CSYSPWRUPREQ
           )) goto error;

    /* Read status register. */
    if (SWD_TRANS_DP_READ(s, SWD_DP_RD_CTRLSTAT)) goto error;
    // FIXME: check SWD_CTRLSTAT_CSYSPWRUPACK | SWD_CTRLSTAT_CDBGPWRUPACK
    infof("ctrlstat 0x%08x, p=%d\n", c->val, !!(c->flags & SWD_FLAGS_PARITY));

    /* Iterate over APs */
    for (s->i = 0; s->i < 4; s->i++) {
        /* Read 0xFC (bank 0xF, reg 0xC) in AP 0 */
        if (SWD_TRANS_DP_WRITE(s, SWD_DP_WR_SELECT,
                         (0xf   << 4)  /* APBANKSEL */
                         |(0    << 0)  /* DPBANKSEL */
                         |(s->i << 24) /* APSEL */
                )) goto error;
        if (SWD_TRANS_AP_READ(s, 0xC)) goto error;
        if (SWD_TRANS_DP_READ(s, SWD_DP_RD_RDBUFF)) goto error;
        /* Assuming zero result means end of AP list */
        if (!c->val) break;
        /* 0x14770011 is AHB-AP */
        infof("idr %d 0x%08x\n", s->i, c->val);
    }

    /* All subsequent AP reads/writes use only the main registers, so
       we can just set SELECT once.  Assume this is MEM-AP. */
    if (SWD_TRANS_DP_WRITE(s, SWD_DP_WR_SELECT,                       \
                      (0   << 4)  /* APBANKSEL */               \
                     |(0   << 0)  /* DPBANKSEL */               \
                     |(0   << 24) /* APSEL */                   \
            )) goto error;

    /* Configure MEM-AP for 32 bit transfers. */
    SWD_AP_REG_READ(s, 0x0); // CSW
    infof("csw 0x%08x\n", c->val);
    SWD_AP_REG_WRITE(s, SWD_MEM_AP_CSW, c->val | 2);
    SWD_AP_REG_READ(s, SWD_MEM_AP_CSW);
    infof("csw 0x%08x\n", c->val);

    /* Perform memory reads and writes */
    SWD_MEM_AP_READ(s, 0x08000000)
    infof("drw 0x%08x\n", c->val);

    SWD_MEM_AP_READ(s, 0x08000004)
    infof("drw 0x%08x\n", c->val);

    SWD_MEM_AP_READ(s, 0x20000000)
    infof("drw 0x%08x\n", c->val);

    SWD_MEM_AP_WRITE(s, 0x20000000, 0x123)

    SWD_MEM_AP_READ(s, 0x20000000)
    infof("drw 0x%08x\n", c->val);



    SM_HALT(s);
  error:
    infof("error\n");
    SM_HALT_STATUS(s, 1);
}


DEF_INSTANCE(swd);


#endif
