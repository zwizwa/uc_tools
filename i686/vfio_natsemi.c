/* hw_i686_dp83815.h as a Linux process, see hw_vfio.h
   Needs a cable: answers arp and ping on the address below.
   Usage: vfio_natsemi.elf 0000:03:02.0 */

#include "hw_vfio.h"
#include "hw_i686_dp83815.h"

static const struct ip_addr my_ip = {{ 10, 1, 54, 222 }};
static struct dp83815 *dp;

static void found(void *ctx, struct pci_function *f) {
    LOG("pci %02x:%02x.%d %04x:%04x\n",
        f->bus, f->dev, f->func, f->vendor, f->device);
    if ((f->vendor == dp83815_vendor) && (f->device == dp83815_device)) {
        dp83815_init(dp, f);
    }
}
static void tx(void *ctx, const uint8_t *data, uint32_t len) {
    int rv = dp83815_transmit(dp, data, len);
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
    if (!dp->mmio) ERROR("no dp83815\n");
    dp->rx = rx;

    for (int i = 0; i < 20; i++) {
        if (vfio_irq_wait(1000)) {
            dp83815_isr_inner(dp);
            vfio_irq_unmask();
        }
    }
    return 0;
}
