#!/bin/sh
cd $(dirname "$0")
# Note that there are build issues with ipxe.lkrn (duplicate symbols).
# Since this is always specific for a host, just do the built-in network card.
# It doesn't need zwizwa.ipxe (which loads ipxe) just menu.ipxe directly.
IMG=80861502.lkrn

./build.sh $IMG deimos-br1.ipxe

DIR=deimos-grub-boot
mkdir -p $DIR
mv src/bin/$IMG $DIR/ipxe
