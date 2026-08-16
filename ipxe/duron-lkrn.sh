#!/bin/sh
cd $(dirname "$0")

# Cloned from portia-lkrn.sh
# 00:03.0 Ethernet controller: Silicon Integrated Systems [SiS] SiS900 PCI Fast Ethernet (rev 90)
# 00:03.0 0200: 1039:0900 (rev 90)


# Note that there are build issues with ipxe.lkrn (duplicate symbols).
# Since this is always specific for a host, just do the built-in network card.
# It doesn't need zwizwa.ipxe (which loads ipxe) just menu.ipxe directly.
IMG=10390900.lkrn

./build.sh $IMG duron-br1.ipxe

DIR=duron-grub-boot
mkdir -p $DIR
mv src/bin/$IMG $DIR/ipxe
