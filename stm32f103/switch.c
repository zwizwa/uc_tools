/* Generic packet switching test application.
   - MDIO Ethernet PHY access
   - SLIP on ttyACM
   TODO:
   - SLIP on UART
   - SPI RPC client
   - SPI RPC server
   - SPI async receive
*/

#include "base.h"
#include "gdbstub_api.h"
#include <string.h>

#include "slipstub.h"

#include "cbuf.h"
#include "pbuf.h"
#include "mdio.h"

/* CONFIG */

// Main rate is just base clock for audio
#define DIV 1500 // (/ 72000000 48000)
#define TIM_PERIODIC 4

#define MDIO_DATA  GPIOA,0
#define MDIO_CLOCK GPIOA,1

static const struct hw_periodic hw_periodic_config[] = {
//          rcc       irq            tim   div  pre
//---------------------------------------------------
    [2] = { RCC_TIM2, NVIC_TIM2_IRQ, TIM2, DIV, 1 },
    [3] = { RCC_TIM3, NVIC_TIM3_IRQ, TIM3, DIV, 1 },
    [4] = { RCC_TIM4, NVIC_TIM4_IRQ, TIM4, DIV, 1 },
    [5] = { RCC_TIM5, NVIC_TIM5_IRQ, TIM5, DIV, 1 },
};
#define C_PERIODIC hw_periodic_config[TIM_PERIODIC]

/*
   SPI modes:
   mode 0: 0RW: Idle at (0), (R)ead edge,  (W)rite edge
   mode 3: 1WR: Idle at (1), (W)rite edge, (R)ead edge

   Below a couple of examples of configuration tables.

*/

/* A note on configuration tables:

   1. The verbose view of multiple configurations seems to be a good
      way to get across what exactly is involved in setting up a
      device.  This is otherwise very hard to document.

   2. These being const struct, they get optimized away in the
      functions that use them, which are static inline.
*/


//          rcc_gpio   rcc_spi   spi   rst       gpio    data clk master tx  hw_dma      ie  bits mode
// ----------------------------------------------------------------------------------------------------------
const struct hw_spi hw_spi_rx16_slave_0rw[] = {
    [1] = { RCC_GPIOA, RCC_SPI1, SPI1, RST_SPI1, GPIOA,  7,   5,  0,     0,  HW_DMA_1_2,  1, 16,  HW_SPI_0RW},
    [2] = { RCC_GPIOB, RCC_SPI2, SPI2, RST_SPI2, GPIOB, 15,  13,  0,     0,  HW_DMA_1_4,  1, 16,  HW_SPI_0RW},
};
const struct hw_spi hw_spi_tx16_slave_0rw[] = {
    [1] = { RCC_GPIOA, RCC_SPI1, SPI1, RST_SPI1, GPIOA,  6,   5,  0,     1,  HW_DMA_1_3,  1, 16,  HW_SPI_0RW},
    [2] = { RCC_GPIOB, RCC_SPI2, SPI2, RST_SPI2, GPIOB, 14,  13,  0,     1,  HW_DMA_1_5,  1, 16,  HW_SPI_0RW},
};
const struct hw_spi hw_spi_tx8_master_0rw[] = {
    [1] = { RCC_GPIOA, RCC_SPI1, SPI1, RST_SPI1, 0,      7,   5,  1,     1,  HW_DMA_1_3,  0,  8,  HW_SPI_0RW},
    [2] = { RCC_GPIOB, RCC_SPI2, SPI2, RST_SPI2, 0,     15,  13,  1,     1,  HW_DMA_1_5,  0,  8,  HW_SPI_0RW},
};
const struct hw_spi hw_spi_rx8_master_0rw[] = {
    [1] = { RCC_GPIOA, RCC_SPI1, SPI1, RST_SPI1, 0,      6,   5,  1,     0,  HW_DMA_1_2,  0,  8,  HW_SPI_0RW},
    [2] = { RCC_GPIOB, RCC_SPI2, SPI2, RST_SPI2, 0,     14,  13,  1,     0,  HW_DMA_1_4,  0,  8,  HW_SPI_0RW},
};
const struct hw_spi hw_spi_tx16_master_0rw[] = {
    [1] = { RCC_GPIOA, RCC_SPI1, SPI1, RST_SPI1, GPIOA,  7,   5,  1,     1,  HW_DMA_1_3,  1, 16,  HW_SPI_0RW}
};

//          rcc_gpio   rcc_spi   spi   rst       gpio   out in  sck  mode
// ------------------------------------------------------------------------------
const struct hw_spi_nodma hw_spi_nodma_master_0rw[] = {
    [1] = { RCC_GPIOA, RCC_SPI1, SPI1, RST_SPI1, GPIOA,  7,  6,  5,  HW_SPI_0RW},
    [2] = { RCC_GPIOB, RCC_SPI2, SPI2, RST_SPI2, GPIOB, 15, 14, 13,  HW_SPI_0RW},
};

/* There are essentially 3 use cases for SPI:

   - RPC client mode, where we are sending requests as clock master,
     and a reply comes back as part of the same transaction.

     This is as good as free because there will already be a SPI line
     for FPGA bootup.  It can be used for low level config and program
     upload.

   - Asynchronous receive.  Data is sent and externally and we have
     clock in slave mode.  This requires a dedicated SPI port.

     This is only necessary when UART SLIP is not fast enough. For
     inter-uC and FPGA communication it makes more sense to use UART
     SLIP.  (USART1 4.5Mbits/s, others 2.25Mbits/s).

   - Asynchronous RPC mode, where we will answer requests from
     outside.  This is less common, but it might be necessary for
     testing.

     This is hard to do in uC, because it requires fast response in
     preparing the reply.  In FPGA it is easy to do for e.g. registers
     or memory, but I've had to add guard clocks before to make it
     easier to implement.
*/


#define C_SPI_RPC    hw_spi_nodma_master_0rw[1]
#define C_SPI_RPC_CS GPIOA,4

#define C_SPI_ASYNC  hw_spi_rx8_slave_0rw[2]



/* STATE VARS & BUFFERS */

static volatile uint32_t sample;
static volatile uint32_t tick;
uint32_t tick_period = 48000;

struct cbuf cbuf_from_usb; uint8_t cbuf_from_usb_buf[4];
struct cbuf cbuf_to_usb;   uint8_t cbuf_to_usb_buf[1024];
struct pbuf pbuf_from_usb; uint8_t pbuf_from_usb_buf[1024];


/* LOW LEVEL HW ROUTINES */

int mdio_get_data() {
    return hw_gpio_read(MDIO_DATA);
}
void mdio_set_data(int v) {
    hw_gpio_write(MDIO_DATA,v);
}
void mdio_set_clock(int v) {
    hw_gpio_write(MDIO_CLOCK,v);
}
void mdio_set_dir(int d) {
    uint32_t config = d ? HW_GPIO_CONFIG_OUTPUT : HW_GPIO_CONFIG_INPUT_PULL;
    hw_gpio_write(MDIO_DATA,1);
    hw_gpio_config(MDIO_DATA,config);
}
void mdio_delay(void) {
    hw_busywait(HW_LOOPS_PER_US/2);
    // hw_busywait_ms(1);
}


static inline void spi_rpc_cs(int val) {
    hw_gpio_write(C_SPI_RPC_CS, val);
}
static inline uint8_t spi_rpc_byte(uint8_t b) {
    return hw_spi_nodma_rdwr(C_SPI_RPC, b);
}



/* ISR */

void HW_TIM_ISR(TIM_PERIODIC)(void) {
    hw_periodic_ack(C_PERIODIC);
    sample++;
    if (sample == tick_period) {
        sample = 0;
        tick++;
    }
}



/* APPLICATION */

/* An RPC transaction has 3 phases:
   - Command.  The server will know how to implement.
   - Gap.      Give server time to prepare the reply.
   - Reply.    Gather reply clocked out by server.
*/
void spi_rpc(
    const uint8_t *cmd, uint32_t cmd_len,
    uint32_t gap_len,
    uint8_t *res, uint32_t res_len) {

    spi_rpc_cs(0);
    for(uint32_t i=0; i<cmd_len; i++) { spi_rpc_byte(cmd[i]); }
    for(uint32_t i=0; i<gap_len; i++) { spi_rpc_byte(0); }
    for(uint32_t i=0; i<res_len; i++) { res[i] = spi_rpc_byte(0); }
    spi_rpc_cs(1);
}

KEEP void set_pin(int pin, int val) {
    hw_gpio_write(GPIOA,pin,val);
    hw_gpio_config(GPIOA,pin,HW_GPIO_CONFIG_OUTPUT);
}
uint32_t nb_commands = 0;

/* SLIP data incoming from USB controller.
   Called by USB driver.
   Packets end ip in dispatch() */


static void dispatch(struct slipstub *s, uint16_t tag, const struct pbuf *p) {
    switch(tag) {
    case 0:
        /* pin write */
        if (p->count < 4) return;
        set_pin(p->buf[2], p->buf[3]);
        cbuf_write_slip_reply(&cbuf_to_usb, p, 4, 0, 0);
    case 1: {
        /* PHY reg write */
        if (p->count < 6) return;
        uint8_t  phy = *(int*)(p->buf+2);
        uint8_t  reg = *(int*)(p->buf+3);
        uint16_t val = *(int*)(p->buf+4);
        infof("mdio_write %d %d 0x%04x\n", phy, reg, val);
        mdio_write(phy, reg, val);
        cbuf_write_slip_reply(&cbuf_to_usb, p, 6, 0, 0);
        break;
    }
    case 2: {
        /* PHY reg read */
        if (p->count < 4) return;
        uint8_t  phy = *(int*)(p->buf+2);
        uint8_t  reg = *(int*)(p->buf+3);
        uint16_t val = mdio_read(phy, reg);
        infof("mdio_read %d %d -> 0x%04x\n", phy, reg, val);
        cbuf_write_slip_reply(&cbuf_to_usb, p, 4, (uint8_t*)&val, sizeof(val));
        break;
    }
    case 3: {
        /* SPI RPC */
        // gdbstub_hub:call(bp1,<<0,3,1,1,1,123>>,1000).
        const uint8_t *cmd = p->buf+5;
        uint8_t cmd_len = p->buf[2];
        uint8_t gap_len = p->buf[3];
        uint8_t res_len = p->buf[4];
        uint8_t res[res_len];
        spi_rpc(cmd, cmd_len, gap_len, res, res_len);
        cbuf_write_slip_reply(&cbuf_to_usb, p, 5+cmd_len, res, res_len);
        break;
    }
    default:
        infof("bad tag %04x", tag);
    }
}

static int link(void) {
    return (mdio_read(1, 1) >> 2)&1;
}
uint32_t last_tick;
int last_link;
static void poll(void) {
    /* Note that there can be no direct path from recv() to a log
     * flush message going out, as we will receive our own broadcasts
     * and this would create a feedback loop.  Limit the rate by
     * polling. */
    if (tick != last_tick) {
        last_tick++;
        //infof("tick\n");
        int cur_link = link();
        if (cur_link != last_link) {
            last_link=cur_link;
            infof("link %d\n", cur_link);
        }
    }
}


/* GDBSTUB / SLIPSTUB BOILERPLATE */

struct slipstub slipstub = {
    .slip_in   = &cbuf_from_usb,
    .packet_in = &pbuf_from_usb,
    .slip_out  = &cbuf_to_usb,
    .dispatch  = dispatch
};

void start(void) {
    hw_app_init();

    rcc_periph_clock_enable(RCC_GPIOA | RCC_AFIO);

    CBUF_INIT(cbuf_from_usb);
    CBUF_INIT(cbuf_to_usb);
    PBUF_INIT(pbuf_from_usb);

    hw_gpio_config(MDIO_CLOCK,HW_GPIO_CONFIG_OUTPUT);

    _service.add(poll);

    hw_periodic_init(C_PERIODIC);

    /* Clock measures at 36MHz */
    hw_spi_nodma_init(C_SPI_RPC);
    hw_gpio_write(C_SPI_RPC_CS, 1);
    hw_gpio_config(C_SPI_RPC_CS,HW_GPIO_CONFIG_OUTPUT);

    uint16_t r2 = mdio_read(1, 2);
    uint16_t r3 = mdio_read(1, 3);
    infof("phy: ID %04x:%04x\n", r2, r3);
    infof("phy: link=%d\n", link());

}
void stop(void) {
    hw_app_stop();
    _service.reset();
}

const char config_manufacturer[] CONFIG_DATA_SECTION = "Zwizwa";
const char config_product[]      CONFIG_DATA_SECTION = "Packet switching test board";
const char config_firmware[]     CONFIG_DATA_SECTION = FIRMWARE;
const char config_protocol[]     CONFIG_DATA_SECTION = "slip";

struct gdbstub_config config CONFIG_HEADER_SECTION = {
    .manufacturer    = config_manufacturer,
    .product         = config_product,
    .firmware        = config_firmware,
    .version         = config_version,
    .protocol        = config_protocol,
    .start           = start,
    .stop            = stop,
    .switch_protocol = slipstub_switch_protocol,
};
