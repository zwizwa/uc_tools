#!/bin/sh

HERE=$(readlink -f $(dirname "$0"))

# Start qemu with default options, leaving only serial port unspecfied.
[ -z "$1" ] && SERIAL="-serial stdio"

DISPLAY_OPT="sdl,gl=on"
# DISPLAY_OPT="gtk,zoom-to-fit=on"


DSK=ipxe-qemu.dsk


export TERM=dumb
exec qemu-system-i386 \
-m 16 \
-device isa-debug-exit,iobase=0xf4,iosize=0x04 \
-drive file=$HERE/$DSK,format=raw,if=floppy,file.locking=off \
-boot a \
-netdev user,id=net0,tftp=. \
-device rtl8139,netdev=net0 \
-display $DISPLAY_OPT \
$SERIAL \
"$@"
