#!/usr/bin/env bash
# Run on the host with the card, as root.  See hw_vfio.h
A=${1:-0000:03:00.0}
D=/sys/bus/pci/devices/$A

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

bind
$(dirname "$0")/vfio_sunix.elf $A
unbind
