#!/bin/sh
# i686 kernel in qemu with emulated hardware, no passthrough:
# - kernel.dsk on floppy, 64MB for the plugin slot at 32MB
# - ahci with sq.img: 256MB, one type 7f partition
# - NIC (rtl8139 or e1000e) on a private tap: only T/32 routed to it,
#   nothing bridged, the kernel's floppy default ip is 10.1.54.222
# - serial on tcp 127.0.0.1:4556
# - DBG=1: exit instead of reboot on triple fault, cpu reset log in
#   cpu.log, monitor on mon.sock (info registers, pmemsave)
# Run as root in the directory holding kernel.dsk.  Stop with:
#   kill $(pgrep -f "^qemu-system-i386 -name sqvfs")
# (pkill -f without ^ also matches the shell running it over ssh)
NIC=${NIC:-rtl8139}
T=${T:-10.1.54.222}
TAP=tapsq
[ -e sq.img ] || {
    truncate -s 256M sq.img
    echo 'start=2048, type=7f' | sfdisk -q sq.img
}
DBG_ARGS=
[ "$DBG" = 1 ] && DBG_ARGS="-no-reboot -d cpu_reset,guest_errors -D cpu.log -monitor unix:$PWD/mon.sock,server,nowait"
ip tuntap add $TAP mode tap
ip link set $TAP up
ip route add $T/32 dev $TAP
qemu-system-i386 -name sqvfs $DBG_ARGS -enable-kvm -m 64 \
    -drive file=kernel.dsk,format=raw,if=floppy,file.locking=off -boot a \
    -device ahci,id=ahci -drive file=sq.img,format=raw,if=none,id=sq \
    -device ide-hd,drive=sq,bus=ahci.0 \
    -netdev tap,id=n,ifname=$TAP,script=no,downscript=no \
    -device $NIC,netdev=n -display none \
    -serial tcp:127.0.0.1:4556,server,nowait >qemu.log 2>&1
ip link del $TAP
