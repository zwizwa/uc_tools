#!/bin/sh
cd $(dirname "$0")
# Note that there are build issues with ipxe.lkrn (duplicate symbols).
# Since this is always specific for a host, just do the built-in network card.
# It doesn't need zwizwa.ipxe (which loads ipxe) just menu.ipxe directly.
IMG=8086104a.lkrn

./build.sh $IMG portia-br1.ipxe

DIR=portia-grub-boot
mkdir -p $DIR
mv src/bin/$IMG $DIR/ipxe
