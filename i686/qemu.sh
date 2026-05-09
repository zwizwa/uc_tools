#!/bin/sh
# Start qemu with default options, leaving only serial port unspecfied.
[ -z "$1" ] && SERIAL="-serial stdio"

DISPLAY_OPT="sdl,gl=on"
# DISPLAY_OPT="gtk,zoom-to-fit=on"

export TERM=dumb
exec qemu-system-i386 \
-m 16 \
-drive file=ipxe-qemu.dsk,format=raw,if=floppy \
-boot a \
-netdev user,id=net0,tftp=. \
-device rtl8139,netdev=net0 \
-display $DISPLAY_OPT \
$SERIAL \
"$@"
