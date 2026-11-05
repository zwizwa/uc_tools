#!/usr/bin/env bash
# Run on the host with the card, as root.  See hw_vfio.h
A=${1:-0000:03:02.0}
D=/sys/bus/pci/devices/$A
SMBUS=0000:00:1f.3  # shares the irq, vfio needs it exclusive: no INTx mask
HUGEPAGES=2000      # more than fits above 4G

bind() {
    modprobe vfio enable_unsafe_noiommu_mode=1
    modprobe vfio-pci
    echo $SMBUS >/sys/bus/pci/drivers/i801_smbus/unbind
    echo vfio-pci >$D/driver_override
    echo $A >$D/driver/unbind
    echo $A >/sys/bus/pci/drivers_probe
}
unbind() {
    echo $A >$D/driver/unbind
    echo >$D/driver_override
    echo $A >/sys/bus/pci/drivers_probe
    echo $SMBUS >/sys/bus/pci/drivers/i801_smbus/bind
}

bind
echo $HUGEPAGES >/proc/sys/vm/nr_hugepages
$(dirname "$0")/vfio_natsemi.elf $A
echo 0 >/proc/sys/vm/nr_hugepages
unbind
