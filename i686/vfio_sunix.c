/* hw_i686_sunix.h as a Linux process, see hw_vfio.h
   Nothing needs to be wired to the card: UART runs in internal
   loopback, parallel port reads back its own data register.
   Usage: vfio_sunix.elf 0000:03:00.0 */

#include "hw_vfio.h"
#include "hw_i686_sunix.h"
#include "hw_i686_par.h"

#define MCR_LOOP 0x10

static struct sunix sunix;
static struct par par;

static void found(void *ctx, struct pci_function *f) {
    LOG("pci %02x:%02x.%d %04x:%04x\n",
        f->bus, f->dev, f->func, f->vendor, f->device);
    if ((f->vendor == sunix_vendor) && (f->device == sunix_device)) {
        sunix_init(&sunix, f);
        par.spp = pci_function_bar1_io(f);
    }
}
static void rx(void *ctx, uint8_t byte) {
    LOG("rx %02x\n", byte);
}

int main(int argc, char **argv) {
    if (argc != 2) ERROR("usage: %s <pci address>\n", argv[0]);
    vfio_open(argv[1]);
    struct pci_cb cb = { .fun = found };
    pci_enumerate(&cb);
    if (!sunix.uart.iobase) ERROR("no sunix\n");

    uint16_t io = sunix.uart.iobase;
    outb(io + UART_MCR, inb(io + UART_MCR) | MCR_LOOP);
    while (vfio_irq_wait(100)) vfio_irq_unmask();  // drain sunix_init output
    uart_isr(&sunix.uart, rx, NULL);

    for (uint8_t c = 'a'; c < 'd'; c++) {
        uart_write_byte(&sunix.uart, c);
        int nb_irq = vfio_irq_wait(1000);
        LOG("tx %02x nb_irq=%d\n", c, nb_irq);
        uart_isr(&sunix.uart, rx, NULL);
        vfio_irq_unmask();
    }
    outb(io + UART_MCR, inb(io + UART_MCR) & ~MCR_LOOP);

    for (int i = 0; i < 2; i++) {
        uint8_t val = i ? 0xAA : 0x55;
        par_write_data(&par, val);
        LOG("par %02x -> %02x\n", val, inb(par.spp + SPP_DATA));
    }
    return 0;
}
