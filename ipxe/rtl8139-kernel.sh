#!/bin/sh
# Small boot floppy for RTL8139 loading kernel.nbp
cd $(dirname "$0")
IMG=rtl8139.dsk
./build.sh $IMG kernel.ipxe
mv src/bin/$IMG rtl8139-kernel.dsk


