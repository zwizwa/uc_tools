#!/bin/sh
# Small boot floppy for qemu to quickly load a fixed file.  This seems
# the fastest way to reload an updated bare metal binary.
set -x
cd $(dirname $0)/src
# IMG=bin/undionly.dsk
# IMG=bin/ipxe.dsk
# IMG=bin/virtio-net.dsk
IMG=bin/rtl8139.dsk

make $IMG EMBED=../qemu.ipxe
mv $IMG /i/exo/uc_tools/i686/ipxe-qemu.dsk

make $IMG EMBED=../qemu-br1.ipxe
mv $IMG /i/exo/uc_tools/i686/ipxe-qemu-br1.dsk

