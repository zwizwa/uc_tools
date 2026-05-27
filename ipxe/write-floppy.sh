#!/bin/sh
[ -z "$1" ] && echo "usage: <img>" && exit 1
FLOPPY=$(readlink -f /dev/disk/by-id/usb-TEAC_TEAC_FD-05PUB)
[ ! -b "$FLOPPY" ] && echo "$FLOPPY is not a block device" && exit 1
dd if=$1 of=$FLOPPY

