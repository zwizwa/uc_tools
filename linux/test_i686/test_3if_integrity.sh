#!/bin/sh
# 3if upload arrives byte for byte, qemu_ahci.sh with DBG=1 only.
#   test_3if_integrity.sh [<qemu dir>]
# Pushes kernel.3if to 10.1.54.222 and compares the image at 16MB
# (after the 512 byte trampoline) with kernel.bin via the monitor.
# The ip patched into the image is the floppy default, so no diff.
#
# Found the rtl8139 64K ring bug (WRAP ignored at 64K, one stale
# packet per ring lap).  To see the crc catch it: build a kernel.dsk
# with RCR_RBLEN 3 in hw_i686_rtl8139.h, boot that in the qemu dir,
# then this test reports missing chunks, retransmits, and still no
# diff.
. "$(dirname "$0")/lib.sh"
T=10.1.54.222
Q=${1:-.}

tu send "$I686/kernel.3if" | sort | uniq -c
sleep 1
S=$(stat -c %s "$I686/kernel.bin")
{ echo "pmemsave 0x1000200 $S \"$PWD/mem16.bin\""; sleep 1; } | socat - UNIX-CONNECT:$Q/mon.sock >/dev/null
check "image at 16MB matches kernel.bin" cmp -s "$I686/kernel.bin" mem16.bin
rm -f mem16.bin
finish
