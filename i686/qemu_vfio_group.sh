#!/bin/sh
# i686 kernel in qemu with a real PCI device passed through.
#   qemu_vfio_group.sh <dev> [<other devices in its iommu group>]
# All devices go to vfio-pci (vfio needs the whole group), only the
# first is given to the guest, all get their drivers back on exit.
# Needs intel_iommu=on and VT-d in the BIOS (no DMAR table = no groups).
# Serial on tcp 127.0.0.1:4555, lines need \r\n.  Run as root in the
# directory holding kernel.dsk.  Examples from 2026-11-07:
#   X230 ahci:     0000:00:1f.2 0000:00:1f.0 0000:00:1f.3
#   psamathe ox:   0000:01:00.0
# On nfs-root hosts /root is shared: use a per-host directory.
[ -n "$1" ] || { echo "usage: $0 <dev> [<group devs>]"; exit 1; }
DEV=$1
modprobe vfio-pci
for A in "$@"; do
    D=/sys/bus/pci/devices/$A
    echo vfio-pci >$D/driver_override
    [ -e $D/driver ] && echo $A >$D/driver/unbind
    echo $A >/sys/bus/pci/drivers_probe
done
qemu-system-i386 -enable-kvm -m 64 \
    -drive file=kernel.dsk,format=raw,if=floppy,file.locking=off -boot a \
    -nic none -device vfio-pci,host=$DEV,rombar=0 -display none \
    -serial tcp:127.0.0.1:4555,server,nowait >qemu.log 2>&1
for A in "$@"; do
    D=/sys/bus/pci/devices/$A
    echo $A >$D/driver/unbind
    echo >$D/driver_override
    echo $A >/sys/bus/pci/drivers_probe
done
