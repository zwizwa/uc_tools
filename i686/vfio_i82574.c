/* hw_i686_i82574.h as a Linux process, see hw_vfio.h
   Needs a cable: answers arp and ping on the address below.
   Usage: vfio_i82574.elf <pci address> */

#include "hw_vfio.h"
#include "hw_i686_i82574.h"

static const struct ip_addr my_ip = {{ 10, 1, 54, 224 }};
static struct i82574 *dp;

static void found(void *ctx, struct pci_function *f) {
    LOG("pci %02x:%02x.%d %04x:%04x\n",
        f->bus, f->dev, f->func, f->vendor, f->device);
    if ((f->vendor == i82574_vendor) && (f->device == i82574_device)) {
        i82574_init(dp, f);
    }
}
static void tx(void *ctx, const uint8_t *data, uint32_t len) {
    int rv = i82574_transmit(dp, data, len);
    LOG("tx %d rv=%d\n", len, rv);
}
static void rx(void *ctx, const uint8_t *data, uint32_t len) {
    const struct {
        struct mac mac;
        struct ip  ip;
    } *p = (const void*)data;
    uint16_t ethertype = ntohs(p->mac.ethertype);
    if (ethertype == ETHERTYPE_ARP) {
        arp_rx(tx, NULL, data, len, &my_ip, &dp->mac);
    }
    if ((ethertype == ETHERTYPE_IPV4) && (p->ip.protocol == PROTOCOL_ICMP)) {
        icmp_rx(tx, NULL, data, len, &my_ip);
    }
}

int main(int argc, char **argv) {
    if (argc != 2) ERROR("usage: %s <pci address>\n", argv[0]);
    vfio_open(argv[1]);
    dp = vfio_dma_alloc();
    struct pci_cb cb = { .fun = found };
    pci_enumerate(&cb);
    if (!dp->mmio) ERROR("no i82574\n");
    dp->rx = rx;

    for (int i = 0; i < 200; i++) {
        if (vfio_irq_wait(1000)) {
            i82574_isr_inner(dp);
            vfio_irq_unmask();
        }
    }
    return 0;
}
