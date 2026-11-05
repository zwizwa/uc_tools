#ifndef HW_VFIO_H
#define HW_VFIO_H

/* Run hw_i686_*.h drivers unmodified as a Linux process on vfio-pci.

   - replaces hw_i686_io.h, include this first
   - port I/O goes to the BAR that decodes the port
   - 0xCF8/0xCFC is emulated on the vfio config region, so
     hw_i686_pci.h works and enumerates only the one device
   - INTx arrives on an eventfd
   - no MMIO, no DMA yet

   Bind the device, no reboot needed.  noiommu taints the kernel:

   modprobe vfio enable_unsafe_noiommu_mode=1
   modprobe vfio-pci
   D=/sys/bus/pci/devices/0000:03:00.0
   echo vfio-pci >$D/driver_override
   echo 0000:03:00.0 >$D/driver/unbind
   echo 0000:03:00.0 >/sys/bus/pci/drivers_probe

   Give it back: unbind, echo >$D/driver_override, drivers_probe. */

#define HW_I686_IO  // hw_i686_io.h becomes a no-op
#define _FILE_OFFSET_BITS 64  // region offsets are index << 40 with -m32

#include <fcntl.h>
#include <libgen.h>
#include <linux/vfio.h>
#include <poll.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/eventfd.h>
#include <sys/ioctl.h>
#include <unistd.h>

#define LOG(...) fprintf(stderr, __VA_ARGS__)
#define ERROR(...) { LOG(__VA_ARGS__); exit(1); }
#define VFIO_ASSERT(cond) { if (!(cond)) { perror(#cond); exit(1); } }

struct vfio_bar {
    uint32_t port, size;
    uint64_t offset;
};
struct vfio {
    int dev, irq;
    uint32_t bdf;  // CF8 format
    uint32_t cf8;
    uint64_t cfg_offset;
    struct vfio_bar bar[VFIO_PCI_ROM_REGION_INDEX];
};
static struct vfio vfio;

static inline uint64_t vfio_region(uint32_t index, uint32_t *size) {
    struct vfio_region_info info = { .argsz = sizeof(info), .index = index };
    VFIO_ASSERT(0 == ioctl(vfio.dev, VFIO_DEVICE_GET_REGION_INFO, &info));
    if (size) *size = info.size;
    return info.offset;
}

static inline void vfio_rw(uint64_t offset, void *buf, uint32_t n, int write) {
    int rv = write ?
        pwrite(vfio.dev, buf, n, offset) :
        pread(vfio.dev, buf, n, offset);
    VFIO_ASSERT(rv == n);
}

static inline void vfio_io(uint16_t port, void *buf, uint32_t n, int write) {
    if (port == 0xCF8) {
        if (write) memcpy(&vfio.cf8, buf, n);
        else memcpy(buf, &vfio.cf8, n);
        return;
    }
    if ((port & ~3) == 0xCFC) {
        if ((vfio.cf8 & ~0xFF) != vfio.bdf) {
            memset(buf, 0xFF, n);
            return;
        }
        vfio_rw(vfio.cfg_offset + (vfio.cf8 & 0xFC) + (port & 3), buf, n, write);
        return;
    }
    for (int i = 0; i < VFIO_PCI_ROM_REGION_INDEX; i++) {
        struct vfio_bar *b = &vfio.bar[i];
        if (port >= b->port && port - b->port < b->size) {
            vfio_rw(b->offset + port - b->port, buf, n, write);
            return;
        }
    }
    ERROR("port %04x not in a BAR\n", port);
}

#define VFIO_PORT(in, out, type)                                        \
    static inline type in(uint16_t port) {                              \
        type val; vfio_io(port, &val, sizeof(val), 0); return val;      \
    }                                                                   \
    static inline void out(uint16_t port, type val) {                   \
        vfio_io(port, &val, sizeof(val), 1);                            \
    }
VFIO_PORT(inb, outb, uint8_t)
VFIO_PORT(inw, outw, uint16_t)
VFIO_PORT(inl, outl, uint32_t)

static inline void vfio_irq_set(uint32_t flags, int32_t fd) {
    struct {
        struct vfio_irq_set set;
        int32_t fd;
    } s = {
        .set = {
            .argsz = sizeof(s),
            .flags = flags,
            .index = VFIO_PCI_INTX_IRQ_INDEX,
            .count = 1,
        },
        .fd = fd,
    };
    VFIO_ASSERT(0 == ioctl(vfio.dev, VFIO_DEVICE_SET_IRQS, &s));
}

/* INTx is masked when it fires: run the isr, then vfio_irq_unmask. */
static inline int vfio_irq_wait(int timeout_ms) {
    struct pollfd p = { .fd = vfio.irq, .events = POLLIN };
    uint64_t count = 0;
    if (poll(&p, 1, timeout_ms) <= 0) return 0;
    VFIO_ASSERT(sizeof(count) == read(vfio.irq, &count, sizeof(count)));
    return count;
}
static inline void vfio_irq_unmask(void) {
    vfio_irq_set(VFIO_IRQ_SET_DATA_NONE | VFIO_IRQ_SET_ACTION_UNMASK, -1);
}

/* name is the sysfs name, e.g. 0000:03:00.0 */
static inline void vfio_open(char *name) {
    uint32_t bus, dev, func;
    if (3 != sscanf(name, "%*x:%x:%x.%x", &bus, &dev, &func)) {
        ERROR("bad pci address %s\n", name);
    }
    vfio.bdf = (1u << 31) | (bus << 16) | (dev << 11) | (func << 8);

    char path[256], link[256];
    snprintf(path, sizeof(path), "/sys/bus/pci/devices/%s/iommu_group", name);
    ssize_t len = readlink(path, link, sizeof(link) - 1);
    VFIO_ASSERT(len > 0);
    link[len] = 0;
    snprintf(path, sizeof(path), "/dev/vfio/noiommu-%s", basename(link));

    int container = open("/dev/vfio/vfio", O_RDWR);
    int group = open(path, O_RDWR);
    VFIO_ASSERT(container >= 0);
    VFIO_ASSERT(group >= 0);
    VFIO_ASSERT(0 == ioctl(group, VFIO_GROUP_SET_CONTAINER, &container));
    VFIO_ASSERT(0 == ioctl(container, VFIO_SET_IOMMU, VFIO_NOIOMMU_IOMMU));
    vfio.dev = ioctl(group, VFIO_GROUP_GET_DEVICE_FD, name);
    VFIO_ASSERT(vfio.dev >= 0);

    vfio.cfg_offset = vfio_region(VFIO_PCI_CONFIG_REGION_INDEX, NULL);
    for (int i = 0; i < VFIO_PCI_ROM_REGION_INDEX; i++) {
        struct vfio_bar *b = &vfio.bar[i];
        uint32_t bar;
        vfio_rw(vfio.cfg_offset + 0x10 + 4 * i, &bar, sizeof(bar), 0);
        if (bar & 1) {
            b->port = bar & ~3;
            b->offset = vfio_region(i, &b->size);
        }
    }

    vfio.irq = eventfd(0, 0);
    VFIO_ASSERT(vfio.irq >= 0);
    vfio_irq_set(VFIO_IRQ_SET_DATA_EVENTFD | VFIO_IRQ_SET_ACTION_TRIGGER, vfio.irq);
}

#endif
