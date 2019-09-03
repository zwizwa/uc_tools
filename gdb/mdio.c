/* MDIO Ethernet PHY test. */

#include "base.h"
#include "gdbstub_api.h"
#include <string.h>

#include "slipstub.h"

#include "cbuf.h"
#include "pbuf.h"
struct cbuf cbuf_from_usb; uint8_t cbuf_from_usb_buf[4];
struct cbuf cbuf_to_usb;   uint8_t cbuf_to_usb_buf[1024];
struct pbuf pbuf_from_usb; uint8_t pbuf_from_usb_buf[1024];


#if 0

/* Adapted from
   https://raw.githubusercontent.com/spotify/linux/master/drivers/net/phy/mdio-bitbang.c */

/*
 * Bitbanged MDIO support.
 *
 * Author: Scott Wood <scottwood@freescale.com>
 * Copyright (c) 2007 Freescale Semiconductor
 *
 * Based on CPM2 MDIO code which is:
 *
 * Copyright (c) 2003 Intracom S.A.
 *  by Pantelis Antoniou <panto@intracom.gr>
 *
 * 2005 (c) MontaVista Software, Inc.
 * Vitaly Bordug <vbordug@ru.mvista.com>
 *
 * This file is licensed under the terms of the GNU General Public License
 * version 2. This program is licensed "as is" without any warranty of any
 * kind, whether express or implied.
 */

#define MDIO_READ 1
#define MDIO_WRITE 0

#define MDIO_SETUP_TIME 10
#define MDIO_HOLD_TIME 10

/* Minimum MDC period is 400 ns, plus some margin for error.  MDIO_DELAY
 * is done twice per period.
 */
#define MDIO_DELAY 250

/* The PHY may take up to 300 ns to produce data, plus some margin
 * for error.
 */
#define MDIO_READ_DELAY 350

/* MDIO must already be configured as output. */
static void mdiobb_send_bit(struct mdiobb_ctrl *ctrl, int val)
{
	const struct mdiobb_ops *ops = ctrl->ops;

	ops->set_mdio_data(ctrl, val);
	ndelay(MDIO_DELAY);
	ops->set_mdc(ctrl, 1);
	ndelay(MDIO_DELAY);
	ops->set_mdc(ctrl, 0);
}

/* MDIO must already be configured as input. */
static int mdiobb_get_bit(struct mdiobb_ctrl *ctrl)
{
	const struct mdiobb_ops *ops = ctrl->ops;

	ndelay(MDIO_DELAY);
	ops->set_mdc(ctrl, 1);
	ndelay(MDIO_READ_DELAY);
	ops->set_mdc(ctrl, 0);

	return ops->get_mdio_data(ctrl);
}

/* MDIO must already be configured as output. */
static void mdiobb_send_num(struct mdiobb_ctrl *ctrl, u16 val, int bits)
{
	int i;

	for (i = bits - 1; i >= 0; i--)
		mdiobb_send_bit(ctrl, (val >> i) & 1);
}

/* MDIO must already be configured as input. */
static u16 mdiobb_get_num(struct mdiobb_ctrl *ctrl, int bits)
{
	int i;
	u16 ret = 0;

	for (i = bits - 1; i >= 0; i--) {
		ret <<= 1;
		ret |= mdiobb_get_bit(ctrl);
	}

	return ret;
}

/* Utility to send the preamble, address, and
 * register (common to read and write).
 */
static void mdiobb_cmd(struct mdiobb_ctrl *ctrl, int read, u8 phy, u8 reg)
{
	const struct mdiobb_ops *ops = ctrl->ops;
	int i;

	ops->set_mdio_dir(ctrl, 1);

	/*
	 * Send a 32 bit preamble ('1's) with an extra '1' bit for good
	 * measure.  The IEEE spec says this is a PHY optional
	 * requirement.  The AMD 79C874 requires one after power up and
	 * one after a MII communications error.  This means that we are
	 * doing more preambles than we need, but it is safer and will be
	 * much more robust.
	 */

	for (i = 0; i < 32; i++)
		mdiobb_send_bit(ctrl, 1);

	/* send the start bit (01) and the read opcode (10) or write (10) */
	mdiobb_send_bit(ctrl, 0);
	mdiobb_send_bit(ctrl, 1);
	mdiobb_send_bit(ctrl, read);
	mdiobb_send_bit(ctrl, !read);

	mdiobb_send_num(ctrl, phy, 5);
	mdiobb_send_num(ctrl, reg, 5);
}


static int mdiobb_read(struct mii_bus *bus, int phy, int reg)
{
	struct mdiobb_ctrl *ctrl = bus->priv;
	int ret, i;

	mdiobb_cmd(ctrl, MDIO_READ, phy, reg);
	ctrl->ops->set_mdio_dir(ctrl, 0);

	/* check the turnaround bit: the PHY should be driving it to zero */
	if (mdiobb_get_bit(ctrl) != 0) {
		/* PHY didn't drive TA low -- flush any bits it
		 * may be trying to send.
		 */
		for (i = 0; i < 32; i++)
			mdiobb_get_bit(ctrl);

		return 0xffff;
	}

	ret = mdiobb_get_num(ctrl, 16);
	mdiobb_get_bit(ctrl);
	return ret;
}

static int mdiobb_write(struct mii_bus *bus, int phy, int reg, u16 val)
{
	struct mdiobb_ctrl *ctrl = bus->priv;

	mdiobb_cmd(ctrl, MDIO_WRITE, phy, reg);

	/* send the turnaround (10) */
	mdiobb_send_bit(ctrl, 1);
	mdiobb_send_bit(ctrl, 0);

	mdiobb_send_num(ctrl, val, 16);

	ctrl->ops->set_mdio_dir(ctrl, 0);
	mdiobb_get_bit(ctrl);
	return 0;
}

struct mii_bus *alloc_mdio_bitbang(struct mdiobb_ctrl *ctrl)
{
	struct mii_bus *bus;

	bus = mdiobus_alloc();
	if (!bus)
		return NULL;

	__module_get(ctrl->ops->owner);

	bus->read = mdiobb_read;
	bus->write = mdiobb_write;
	bus->priv = ctrl;

	return bus;
}
EXPORT_SYMBOL(alloc_mdio_bitbang);

void free_mdio_bitbang(struct mii_bus *bus)
{
	struct mdiobb_ctrl *ctrl = bus->priv;

	module_put(ctrl->ops->owner);
	mdiobus_free(bus);
}

#endif




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
    case 1:
        if (p->count < 4) return;
        set_pin(p->buf[2], p->buf[3]);
        cbuf_write_slip_reply(&cbuf_to_usb, p, 4);
    default:
        infof("bad tag %04x", tag);
    }
}

uint8_t last_io_state;
static void poll(void) {
    uint8_t io_state =
        hw_gpio_read(GPIOA,0) |
        (hw_gpio_read(GPIOA,1) << 1);
    if (io_state != last_io_state) {
        cbuf_write_slip_tagged(&cbuf_to_usb, 1, &io_state, 1);
        last_io_state = io_state;
    }
}


/* GDBSTUB / SLIP BOILERPLATE */

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
    _service.add(poll);
}
void stop(void) {
    hw_app_stop();
    _service.reset();
}

const char config_manufacturer[] CONFIG_DATA_SECTION = "Zwizwa";
const char config_product[]      CONFIG_DATA_SECTION = "MDIO Test Board";
const char config_serial[]       CONFIG_DATA_SECTION = "2";
const char config_firmware[]     CONFIG_DATA_SECTION = FIRMWARE;
const char config_version[]      CONFIG_DATA_SECTION = BUILD;
const char config_protocol[]     CONFIG_DATA_SECTION = "slip";

struct gdbstub_config config CONFIG_HEADER_SECTION = {
    .manufacturer    = config_manufacturer,
    .product         = config_product,
    .serial          = config_serial,
    .firmware        = config_firmware,
    .version         = config_version,
    .protocol        = config_protocol,
    .start           = start,
    .stop            = stop,
    .switch_protocol = slipstub_switch_protocol,
};



