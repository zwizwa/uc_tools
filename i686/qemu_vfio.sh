#!/usr/bin/env bash
# Run kernel.dsk in qemu on the host with the card, as root.
# Needs intel_iommu=on, vfio-pci refuses to pass through noiommu groups.
A=${1:-0000:03:02.0}
D=/sys/bus/pci/devices/$A
SMBUS=0000:00:1f.3  # shares the irq, vfio needs it exclusive: no INTx mask
HERE=$(dirname "$0")
DSK=$HERE/kernel.dsk
if [ -n "$2" ]; then  # guest ip
    "$HERE/set-ip.sh" "$DSK" "$HERE/kernel.ip.dsk" "$2"
    DSK=$HERE/kernel.ip.dsk
fi

bind() {
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
qemu-system-i386 \
    -enable-kvm \
    -m 16 \
    -drive file=$DSK,format=raw,if=floppy,file.locking=off \
    -boot a \
    -nic none \
    -device vfio-pci,host=$A,rombar=0 \
    -display none \
    -serial stdio
unbind
