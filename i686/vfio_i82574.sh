#!/usr/bin/env bash
# Run on the host with the card, as root.  See hw_vfio.h
# PCIe has an INTx mask, so sharing the irq is fine.
A=$1
D=/sys/bus/pci/devices/$A
HUGEPAGES=2000      # noiommu: more than fits above 4G

bind() {
    modprobe vfio enable_unsafe_noiommu_mode=1
    modprobe vfio-pci
    echo vfio-pci >$D/driver_override
    echo $A >$D/driver/unbind
    echo $A >/sys/bus/pci/drivers_probe
}
unbind() {
    echo $A >$D/driver/unbind
    echo >$D/driver_override
    echo $A >/sys/bus/pci/drivers_probe
}

[ -z "$(ls /sys/kernel/iommu_groups)" ] && NOIOMMU=1
bind
[ -n "$NOIOMMU" ] && echo $HUGEPAGES >/proc/sys/vm/nr_hugepages
$(dirname "$0")/vfio_i82574.elf $A
[ -n "$NOIOMMU" ] && echo 0 >/proc/sys/vm/nr_hugepages
unbind
