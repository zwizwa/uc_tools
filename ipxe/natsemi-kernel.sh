#!/bin/sh
# Small boot floppy for National Semiconductor DP83815 Netgear / Soekris cards
# PCI ID 100b:0020
cd $(dirname "$0")
IMG=natsemi.dsk
./build.sh natsemi.dsk kernel.ipxe
mv src/bin/$IMG natsemi-kernel.dsk
