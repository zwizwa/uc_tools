#ifndef MOD_SWD
#define MOD_SWD

/* Stand-alone Debug Access Port / Serial Wire Debug interface monitor
   tool, with OpenOCD support.

   PROTOCOL

   TL&DR: There are a couple of layers to the protocol.

   Specific to STM32F103, from high level down to low level:

   1. You probably want to only perform MEM-AP access, to read out
      words from the processor's memory space.  This can be done while
      it is running.

   2. Such a memory access uses the CSW (config), TAR (target
      address), and DRW (data read/write) registers in the MEM-AP: set
      TAR, and read DRW.

   3. Each of the individual reads and writes to those registers
      requires two to three SWD : (optionally) select
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
      - write at rising edge, read at falling edge

   LINKS

   https://developer.arm.com/architectures/cpu-architecture/debug-visibility-and-trace/coresight-architecture/serial-wire-debug
   https://en.wikipedia.org/wiki/JTAG#Similar_interface_standards
   https://research.kudelskisecurity.com/2019/05/16/swd-arms-alternative-to-jtag
   https://www.cnblogs.com/shangdawei/p/4748751.html
   https://developer.arm.com/documentation/ddi0316/d/functional-description/swj-dp/swd-and-jtag-selection-mechanism
   https://static.docs.arm.com/ihi0031/c/IHI0031C_debug_interface_as.pdf
   https://research.kudelskisecurity.com/2019/07/31/swd-part-2-the-mem-ap/
   https://developer.arm.com/documentation/ddi0439/b/Debug/About-the-AHB-AP/AHB-AP-programmers-model

   Chapter 31 in RM0008 (TRM STM32F1):
   https://www.st.com/resource/en/reference_manual/cd00171190-stm32f101xx-stm32f102xx-stm32f103xx-stm32f105xx-and-stm32f107xx-advanced-arm-based-32-bit-mcus-stmicroelectronics.pdf

   IMPLEMENTATION

   The SWD bit-level transaction protocol is written as a set of
   macros and is instantiated as:

   - a state machine, i.e. sm.h protothread macros

   - nested function calls for busy bit-bang

   The former allows bit level context switching, which might be
   useful when driving multiple SWD lines at a slower speed.  It was
   the original implementation, but is currently not used as the
   context switch overhead makes it very slow, and the absence of
   function calls makes it awkward to implement a nested protocol.

   The latter provides ordinary C functions to perform the 46 bit SWD
   transactions without context switching. It is the implementation
   used to implement all stand alone console commands, which also
   implement the operations used by the OpenOCD driver.  It is faster
   and more convenient for development.

   For OpenOCD integration, there is a minimal driver that provides a
   1-1 bridge from OpenOCD swd adapter API to text commands over
   ttyACM.  Commands are implemented at the bottom of this file.
   See als openocd/src/jtag/drivers/pdap.c

*/

#include "sm.h"
#include "cbuf.h"

#define SWD_GPIO_SWDIO GPIOA,0
#define SWD_GPIO_SWCLK GPIOA,1
#define SWD_GPIO_SRST  GPIOA,2

#define SWD_OUT 0
#define SWD_IN  1

#if 0

// FIXME: Move STM code elsewhere.

INLINE int  swd_get_swdio(void)     { return hw_gpio_read(SWD_GPIO_SWDIO); }
INLINE void swd_set_swdio(int val)  { hw_gpio_write_v2(SWD_GPIO_SWDIO, val); }
INLINE void swd_swclk(int val)      { hw_gpio_write_v2(SWD_GPIO_SWCLK, val); }
INLINE void swd_srst(int val)       { hw_gpio_write_v2(SWD_GPIO_SRST, val); }
INLINE void swd_dir(int in) {
    hw_gpio_config(
        SWD_GPIO_SWDIO, in ?
        HW_GPIO_CONFIG_INPUT :
        HW_GPIO_CONFIG_OUTPUT);
}
INLINE void swd_busywait(uint32_t ticks) {
    hw_busywait(ticks);
}
INLINE uint32_t swd_cycle_counter_future_time(uint32_t ticks) {
    return cycle_counter_future_time(ticks);
}
INLINE int swd_cycle_counter_expired(uint32_t time) {
    return cycle_counter_expired(time);
}


#else

INLINE int  swd_get_swdio(void);
INLINE void swd_set_swdio(int val);
INLINE void swd_swclk(int val);
INLINE void swd_srst(int val);
INLINE void swd_dir(int in);
INLINE void swd_busywait(uint32_t ticks);
INLINE uint32_t swd_cycle_counter_future_time(uint32_t delta_time);
INLINE int swd_cycle_counter_expired(uint32_t time);

#endif



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


/* Context-switch in every clock period.  It is likely not necessary
   to have such a fine granularity, but it is convenient to
   implement. */
#define SWD_DELAY(s) \
    SM_SUSPEND(s)

/* Precondition for these two macros: clock is low, and delay still
   needed before setting clock high.  Direction needs to be set
   elsewhere. */

//                                       _______________
//                                ______|   w           |___r___
#define SWD_W(dw, dr, c, w, b)  { dw; c(1); w(b); dr; c(0);    }
#define SWD_R(dw, dr, c, r)    ({ dw; c(1);       dr; c(0); r; })

#define SWD_WRITE_BIT(s, bit) SWD_W(SWD_DELAY(s), SWD_DELAY(s), swd_swclk, swd_set_swdio, bit)
#define SWD_READ_BIT(s)       SWD_R(SWD_DELAY(s), SWD_DELAY(s), swd_swclk, swd_get_swdio())


//#define SWD_INFO_BIT infof
#define SWD_INFO_BIT(...)

#define SWD_WRITE_LSB_(s, bits_vec, bits_nb, dr, i, flags, write_bit) { \
        flags &= ~SWD_FLAGS_PARITY;                                     \
        SWD_INFO_BIT("w:");                                             \
        dr = bits_vec;                                                  \
        for (i = 0; i < (bits_nb); i++) {                               \
            if (dr & 1) {                                               \
                flags ^= SWD_FLAGS_PARITY;                              \
            }                                                           \
            SWD_INFO_BIT("%d", dr&1);                                   \
            write_bit(s, dr&1);                                         \
            dr >>= 1;                                                   \
        }                                                               \
        SWD_INFO_BIT("\n");                                             \
    }




#define SWD_WRITE_LSB(s, bits_vec, bits_nb) \
        SWD_WRITE_LSB_(s, bits_vec, bits_nb, s->dr, s->i, s->flags, SWD_WRITE_BIT)


/* Optimized blocking versions.  Note that a single transaction is
   fast enough, so it makes sense to use these as atomics in a
   cooperative multitasking environment.  However, speed isn't an
   issue yet, so these are not used. */

#define SWD_READ_LSB_(s, bits_nb, dr, i, flags) ({            \
            flags &= ~SWD_FLAGS_PARITY;                       \
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
            dr;                                               \
        })

#define SWD_READ_LSB(s, bits_nb) \
    SWD_READ_LSB_(s, bits_nb, s->dr, s->i, s->flags)

#define SWD_ONES_ZEROS_(s,n1,n0,i,write_bit) {             \
        for (i = 0; i<n1; i++) { write_bit(s, 1); }        \
        for (i = 0; i<n0; i++) { write_bit(s, 0); }        \
    }
#define SWD_ONES_ZEROS(s,n1,n0) \
    SWD_ONES_ZEROS_(s,n1,n0,s->i,SWD_WRITE_BIT)


/* Reset.  The number of low bits after the 50 x high reset
   sequence is significant.  The first has to be 0x, the second has to
   be 2x. */
#define SWD_RESET_(s,write_lsb,ones_zeros) {                       \
        swd_dir(SWD_OUT);                                               \
        ones_zeros(s,50,0);                                             \
        write_lsb(s, 0xE79E, 16);                                       \
        ones_zeros(s,50,2);                                             \
    }
#define SWD_RESET(s) \
    SWD_RESET_(s,SWD_WRITE_LSB,SWD_ONES_ZEROS)

void log_cmd(uint8_t cmd) {
    LOG("port = %d\n", 1 & (cmd >> 1));
    LOG("read = %d\n", 1 & (cmd >> 2));
    LOG("addr = %d\n", 0b1100 & (cmd >> 1));
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
#define SWD_DP_RD_RDBUFF   0xc

#define SWD_DP_WR_ABORT    0x0
#define SWD_DP_WR_CTRLSTAT 0x4
#define SWD_DP_WR_SELECT   0x8


#define SWD_CTRLSTAT_CDBGRSTREQ (1 << 26)
#define SWD_CTRLSTAT_CDBGRSTACK (1 << 27)
#define SWD_CTRLSTAT_CDBGPWRUPREQ (1 << 28)
#define SWD_CTRLSTAT_CDBGPWRUPACK (1 << 29)
#define SWD_CTRLSTAT_CSYSPWRUPREQ (1 << 30)
#define SWD_CTRLSTAT_CSYSPWRUPACK (1 << 31)

#define SWD_ABORT_STKCMPCLR       (1 << 1)
#define SWD_ABORT_STKERRCLR       (1 << 2)
#define SWD_ABORT_WDERRCLR        (1 << 3)
#define SWD_ABORT_ORUNERRCLR      (1 << 4)

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



/* Transaction based on the _LSB macros.  Note that read and write
   cycles are both 46 clocks, but the location of the turn phases is
   different.  At least one idle cycle is necessary at the end. */

/* The core routine is factored out in terms of direct members.  GCC
   doesn't seem to be smart enough to (or doesn't have enough
   information such that it can) eliminate the state struct.  The only
   references to s that are left are only necessary when this is
   compiled to SM. */
#define SWD_TRANSACTION_(s,cmd,ack,flags,val,write_bit,read_bit,write_lsb,read_lsb) { \
        flags = 0;                                                      \
        swd_dir(SWD_OUT);                                               \
        write_lsb(s, cmd, 8);                                           \
        swd_dir(SWD_IN);                                                \
        read_bit(s); /* turn */                                         \
        ack = read_lsb(s, 3);                                           \
        flags &= ~SWD_FLAGS_PARITY; /* reset */                         \
        if (cmd & (1 << 2)) {                                           \
            /* Read */                                                  \
            val = read_lsb(s, 32);                                      \
            int parity = read_bit(s);                                   \
            SWD_INFO_BIT("p:%d\n", parity);                             \
            if (parity) flags ^= SWD_FLAGS_PARITY;                      \
            read_bit(s); /* turn */                                     \
        }                                                               \
        else {                                                          \
            /* Write */                                                 \
            read_bit(s); /* turn */                                     \
            swd_dir(SWD_OUT);                                           \
            write_lsb(s, val, 32);                                      \
            SWD_INFO_BIT("p:%d\n", !!(flags & SWD_FLAGS_PARITY));       \
            write_bit(s, !!(flags & SWD_FLAGS_PARITY));;                \
        }                                                               \
        /* RM0008 says it needs two after write, but that doesn't work? */ \
        write_bit(s, 0); /* idle */                                     \
        swd_dir(SWD_OUT);                                               \
    }

#define SWD_TRANSACTION(s)                                              \
    SWD_TRANSACTION_(s,s->cmd,s->res,s->flags,s->val,                   \
                     SWD_WRITE_BIT, SWD_READ_BIT, SWD_WRITE_LSB, SWD_READ_LSB)


static inline sm_status_t swd_cmd_tick(struct swd_cmd *s) {
    SM_RESUME(s);
    //infof("swd_cmd start %x\n", s->cmd);
    switch(s->cmd) {
    case 0:
        SWD_RESET(s)
        /* Perform mandatory READID
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





/* High level command server. */
struct swd_serv {
    void *next;
    int16_t i,j;
    struct cbuf cmd,resp;
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


#define SWD_SELECT_(s, ap, reg, trans_dp_write) \
    trans_dp_write(s, SWD_DP_WR_SELECT, \
                   ((reg >> 4) << 4) /* APBANKSEL */     \
                   |(0    << 0)      /* DPBANKSEL */     \
                   |(ap   << 24)     /* APSEL */ )
#define SWD_SELECT(s, ap, reg) \
    SWD_SELECT_(s, ap, reg, SWD_TRANS_DP_WRITE)

int swd_req = 0;

sm_status_t swd_serv_tick(struct swd_serv *s) {
    struct swd_cmd *c = &s->sub.swd_cmd;
    SM_RESUME(s);
    /* Reset SWD. */
    if (SM_SUB_CATCH(s, swd_cmd, 0, 0)) goto error;
    // stm32f103 is 1ba01477
    infof("dpidr 0x%08lx, p=%d\n", c->val, !!(c->flags & SWD_FLAGS_PARITY));

    /* Read status register. */
    if (SWD_TRANS_DP_READ(s, SWD_DP_RD_CTRLSTAT)) goto error;
    infof("ctrlstat 0x%08lx, p=%d\n", c->val, !!(c->flags & SWD_FLAGS_PARITY));

    /* Power up debug domain.*/
    if (SWD_TRANS_DP_WRITE(s, SWD_DP_WR_CTRLSTAT,
                     SWD_CTRLSTAT_CDBGPWRUPREQ | SWD_CTRLSTAT_CSYSPWRUPREQ
           )) goto error;

    /* Read status register. */
    if (SWD_TRANS_DP_READ(s, SWD_DP_RD_CTRLSTAT)) goto error;
    // FIXME: check SWD_CTRLSTAT_CSYSPWRUPACK | SWD_CTRLSTAT_CDBGPWRUPACK
    infof("ctrlstat 0x%08lx, p=%d\n", c->val, !!(c->flags & SWD_FLAGS_PARITY));

    /* Iterate over APs */
    for (s->i = 0; s->i < 4; s->i++) {
        /* Read 0xFC (bank 0xF, reg 0xC) in AP 0 */
        if (SWD_SELECT(s, s->i, 0xFC)) goto error;
        if (SWD_TRANS_AP_READ(s, 0xC)) goto error;
        if (SWD_TRANS_DP_READ(s, SWD_DP_RD_RDBUFF)) goto error;
        /* Assuming zero result means end of AP list */
        if (!c->val) break;
        /* 0x14770011 is AHB-AP */
        infof("ap %d, idr 0x%08lx\n", s->i, c->val);
    }

    /* All subsequent AP reads/writes use only the main registers, so
       we can just set SELECT once.  Assume this is MEM-AP. */
    if (SWD_SELECT(s, 0, 0)) goto error;

    /* Configure MEM-AP for 32 bit transfers and auto-increment.  Note
       that this wraps withing a 1KB address boundary. */
    SWD_AP_REG_READ(s, 0x0); // CSW
    infof("csw 0x%08lx\n", c->val);
    SWD_AP_REG_WRITE(s, SWD_MEM_AP_CSW, c->val | (2 << 0) | (2 << 4));
    SWD_AP_REG_READ(s, SWD_MEM_AP_CSW);
    infof("csw 0x%08lx\n", c->val);

    /* Perform memory reads and writes */
    SWD_MEM_AP_READ(s, 0x08000000)
    infof("drw 0x%08lx\n", c->val);

    SWD_MEM_AP_READ(s, 0x08000004)
    infof("drw 0x%08lx\n", c->val);

    SWD_MEM_AP_READ(s, 0x20000000)
    infof("drw 0x%08lx\n", c->val);

    SWD_MEM_AP_WRITE(s, 0x20000000, 0x123)

    SWD_MEM_AP_READ(s, 0x20000000)
    infof("drw 0x%08lx\n", c->val);


    SWD_MEM_AP_READ(s, 0x08000000)
    infof("drw 0x%08lx (verify 0x08000000)\n", c->val);

    SWD_MEM_AP_READ(s, 0x08000010)
    infof("drw 0x%08lx (verify 0x08000010)\n", c->val);


  again:
    /* Auto-incrementing read.  Note that AP register reads are
       pipelined, so the correct sequence is to set the TAR, read DRW
       once, discard, then read again to obtain the first result.
       Also note that this wraps on 0x400 (1K) boundaries.  The
       following is expanded into individual transactions to make it
       clear what happens.  It assumes SWD is currently set up, CSW is
       configured for incremental word read, and the the MEM_AP base
       registers are selected so we can access TAR and DRW
       directly. */
    SWD_TRANS_AP_WRITE(s, SWD_MEM_AP_TAR, 0x08000000);
    SWD_TRANS_DP_READ(s, SWD_DP_RD_CTRLSTAT); // status read seems necessary
    SWD_TRANS_AP_READ(s, SWD_MEM_AP_DRW); // skip pipeline junk
    for (s->i=0; s->i<300; s->i++) {
        if (SWD_TRANS_AP_READ(s, SWD_MEM_AP_DRW)) goto error;
        infof("drw 0x%08lx @%x %d\n", c->val, 0x08000000 + s->i * 4, s->i);
    }

    for(;;) {
        SM_SUSPEND(s);
        if (swd_req) {
            swd_req--;
            goto again;
        }
    }

    SM_HALT(s);
  error:
    infof("error\n");
    SM_HALT_STATUS(s, 1);
}



#undef SWD_DELAY


// FIXME: This doesn't work properly.  DP reads work, but AP reads do
// not.  It's currently not really necessary so just turn it off.
#if 0

#define CLOCK_HALF_KHZ(khz) (72000 / ((khz) * 2))

uint32_t clock_half = CLOCK_HALF_KHZ(1000);

/* Synchronous. */
static inline void delay_half(void) {
    if (0) {
        swd_busywait(100);
    }
    else {
        uint32_t time = swd_cycle_counter_future_time(clock_half);
        while(!swd_cycle_counter_expired(time));
    }
}

#define SWD_DELAY(s) delay_half()

/* This is the (error) context used for the blocking calls. */
struct swd_ctx {
    void *next; // dummy
    uint32_t flags;
    uint8_t res;
};
void swd_write_bit(struct swd_ctx *c, int bit) {
    SWD_WRITE_BIT(c, bit);
}
int swd_read_bit(struct swd_ctx *c) {
    return SWD_READ_BIT(c);
}
void swd_log_flags(struct swd_ctx *c) {
    if (c->flags) {
        // FIXME: need error mechanism
        infof("ERROR: flags = 0x%08lx\n", c->flags);
    }
}
void swd_write_lsb(struct swd_ctx *c, uint32_t bits_vec, uint32_t bits_nb) {
    uint32_t dr=0, i=0, flags=0;
    SWD_WRITE_LSB_(c, bits_vec, bits_nb, dr, i, flags, swd_write_bit);
    c->flags = flags;
}
uint32_t swd_read_lsb(struct swd_ctx *c, uint32_t bits_nb) {
    uint32_t dr=0, i=0, flags=0;
    SWD_READ_LSB_(c, bits_nb, dr, i, flags);
    c->flags = flags;
    return dr;
}
void swd_ones_zeros(struct swd_ctx *c, uint32_t n1, uint32_t n0) {
    uint32_t i=0;
    SWD_ONES_ZEROS_(c,n1,n0,i,swd_write_bit);
}
void swd_reset(struct swd_ctx *c) {
    SWD_RESET_(c,swd_write_lsb,swd_ones_zeros);
}
uint32_t swd_transaction(struct swd_ctx *c, uint8_t cmd, uint32_t val) {
    SWD_TRANSACTION_(c,cmd,c->res,c->flags,val,
                     swd_write_bit,swd_read_bit,swd_write_lsb,swd_read_lsb);
    return val;
}
uint32_t swd_cmd(struct swd_ctx *c, uint8_t cmd, uint32_t val) {
    if (!cmd) {
        swd_reset(c);
        cmd = swd_cmd_hdr(SWD_PORT_DP, SWD_READ, SWD_DP_RD_DPIDR);
    }
    return swd_transaction(c, cmd, val);
}

uint32_t swd_trans_dp_read(struct swd_ctx *c, uint32_t reg) {
    return swd_cmd(c, swd_cmd_hdr(SWD_PORT_DP, SWD_READ, reg), 0);
}
uint32_t swd_trans_ap_read(struct swd_ctx *c, uint32_t reg) {
    return swd_cmd(c, swd_cmd_hdr(SWD_PORT_AP, SWD_READ, reg), 0);
}
void swd_trans_dp_write(struct swd_ctx *c, uint32_t reg, uint32_t val) {
    swd_cmd(c, swd_cmd_hdr(SWD_PORT_DP, SWD_WRITE, reg), val);
}
void swd_trans_ap_write(struct swd_ctx *c, uint32_t reg, uint32_t val) {
    swd_cmd(c, swd_cmd_hdr(SWD_PORT_AP, SWD_WRITE, reg), val);
}
uint32_t swd_ap_reg_read(struct swd_ctx *c, uint32_t reg) {
    swd_trans_ap_read(c, reg & 0b1100);
    return swd_trans_dp_read(c, SWD_DP_RD_RDBUFF);
}
void swd_ap_reg_write(struct swd_ctx *c, uint32_t reg, uint32_t val) {
    swd_trans_ap_write(c, reg & 0b1100, val);
    swd_trans_dp_read(c, SWD_DP_RD_CTRLSTAT);
}
uint32_t swd_mem_ap_read(struct swd_ctx *c, uint32_t addr) {
    swd_ap_reg_write(c, SWD_MEM_AP_TAR, addr);
    return swd_ap_reg_read(c, SWD_MEM_AP_DRW);
}
void swd_mem_ap_write(struct swd_ctx *c, uint32_t addr, uint32_t val) {
    swd_ap_reg_write(c, SWD_MEM_AP_TAR, addr);
    swd_ap_reg_write(c, SWD_MEM_AP_DRW, val);
}
void swd_select(struct swd_ctx *c, uint32_t ap, uint32_t reg) {
    SWD_SELECT_(c, ap, reg, swd_trans_dp_write);
}

#undef SWD_DELAY

void clear_sticky(struct swd_ctx *c) {
    swd_trans_dp_write(
        c, SWD_DP_WR_ABORT,
        SWD_ABORT_STKCMPCLR | SWD_ABORT_STKERRCLR |
        SWD_ABORT_WDERRCLR  | SWD_ABORT_ORUNERRCLR);
}

#define ESC 033
void cls(void) {
    char c[] = {
        ESC,'[','2','J', // clear screen
        ESC,'[','H',     // move cursor to upper left
        0};
    info_puts(c);
}


#endif


// The DEF_COMMAND stuff also depends on linker scripts, so leave it
// optional.
#if 0
/* Generic interactive command wrappers. */
void swd_command_read(uint32_t (*read)(struct swd_ctx *c, uint32_t addr)) {
    struct swd_ctx c = {};
    uint32_t addr = command_stack_pop();
    uint32_t rv = read(&c, addr);
    command_stack_push(rv);
}
void swd_command_write(void (*write)(struct swd_ctx *c, uint32_t addr, uint32_t val)) {
    struct swd_ctx c = {};
    uint32_t val  = command_stack_pop();
    uint32_t addr = command_stack_pop();
    write(&c, addr, val);
}
uint8_t openocd_cmd_pop(void) {
    /* AP/DP, R/W, Addr, Parity are set by OpenOCD.
       We just need to add Start, Park. */
    return command_stack_pop() | 0x81;
}


DEF_COMMAND(log_cmd) { log_cmd(command_stack_pop()); }
DEF_COMMAND(swd_req) {
    swd_req++;
}
DEF_COMMAND(rd) {
    uint8_t cmd = openocd_cmd_pop();
    struct swd_ctx c = {};
    uint32_t rv;
  again:
    rv = swd_cmd(&c, cmd, 0);
    if (c.res == 1) { // OK
    }
    else {
        if (c.res == 2) { // WAIT
            LOG("# wait\n");
            clear_sticky(&c);
            goto again;
        }
        LOG("# res %x\n", c.res);
    }
    command_stack_push(rv);
}
DEF_COMMAND(wr) {
    uint8_t cmd  = openocd_cmd_pop();
    uint32_t arg = command_stack_pop();
    struct swd_ctx c = {};
  again:
    swd_cmd(&c, cmd, arg);
    if (c.res == 1) { // OK
    }
    else {
        if (c.res == 2) { // WAIT
            LOG("# wait\n");
            clear_sticky(&c);
            goto again;
        }
        LOG("# res %x\n", c.res);
    }
}
DEF_COMMAND(srst) {
    uint32_t arg = command_stack_pop();
    // LOG("# %x srst\n", arg);
    swd_srst(!arg);
}
DEF_COMMAND(swd_cmd) {
    struct swd_ctx c = {};
    uint32_t wval = command_stack_pop();
    uint8_t  cmd  = command_stack_pop();
    uint32_t rval = swd_cmd(&c, cmd, wval);
    LOG("rval = 0x%08lx\n", rval);
    command_stack_push(rval);
}
/* OpenOCD commands.

   openocd/src/jtag/drivers/pdap.c uses a direct mapping of OpenOCD
   callbacks to RPN commands.

*/
#define SWD_DEF_READ_COMMAND(fname,cname) \
    DEF_COMMAND(fname) { swd_command_read(cname); }

#define SWD_DEF_WRITE_COMMAND(fname,cname) \
    DEF_COMMAND(fname) { swd_command_write(cname); }

SWD_DEF_READ_COMMAND(dp_rd,swd_trans_dp_read);
SWD_DEF_READ_COMMAND(ap_rd,swd_trans_ap_read);
SWD_DEF_WRITE_COMMAND(dp_wr,swd_trans_dp_write);
SWD_DEF_WRITE_COMMAND(ap_wr,swd_trans_ap_write);
SWD_DEF_READ_COMMAND(ap_req_rd,swd_ap_reg_read)
SWD_DEF_WRITE_COMMAND(ap_reg_wr,swd_ap_reg_write)
SWD_DEF_READ_COMMAND(mem_rd,swd_mem_ap_read)
SWD_DEF_WRITE_COMMAND(mem_wr,swd_mem_ap_write)

DEF_COMMAND(select) {
    struct swd_ctx c = {};
    uint32_t reg = command_stack_pop();
    uint32_t ap  = command_stack_pop();
    swd_select(&c, ap, reg);
}

DEF_COMMAND(sync) {
    LOG("sync %x\n", command_stack_pop());
}
DEF_COMMAND(jtag_to_swd) {
    struct swd_ctx c = {}; swd_cmd(&c, 0, 0);
}
DEF_COMMAND(swd_to_jtag) {
}
DEF_COMMAND(line_reset) {
}
DEF_COMMAND(idle) {
    uint32_t nb_zeros = command_stack_pop();
    struct swd_ctx c = {};
    swd_dir(SWD_OUT);
    swd_ones_zeros(&c, 0, nb_zeros);
    //swd_dir(SWD_IN);
}
COMMAND_REGISTER_NAMED("cls", cls);

#endif



// The instance.h doesn't seem like a good idea for an API because it
// is too specific to projects where there is full control over the
// linker scripts.  Also, mod_swd.c should be platform-independent.

#if 0
#include "instance.h"

instance_status_t swd_init(instance_init_t *ctx) {
    int clk = hw_gpio_read(SWD_GPIO_SWCLK);
    int dio = hw_gpio_read(SWD_GPIO_SWDIO);
    infof("swd_init clk=%d, dio=%d\n", clk, dio);

    swd_swclk(0); hw_gpio_config(SWD_GPIO_SWCLK, HW_GPIO_CONFIG_OUTPUT);
    swd_srst(1);  hw_gpio_config(SWD_GPIO_SRST,  HW_GPIO_CONFIG_OUTPUT);
    swd_set_swdio(1);
    swd_dir(SWD_OUT);
    return 0;
}
DEF_INSTANCE(swd);
#endif



#endif
