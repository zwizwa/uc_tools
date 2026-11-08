#!/bin/sh
# COM1 console output is interrupt driven, see uart_tx in hw_i686_com.h.
#   test_com1_tx.sh <qemu dir>
# Starts qemu_ahci.sh in <qemu dir> (kernel.dsk there), captures COM1
# from boot, sends 600 console lines over UDP (about 20K of echo plus
# output at 115200 baud, so the 4K tx ring fills and the main loop
# waits for room).  Checks:
# - boot log on COM1: written with interrupts off, before the pic is
#   set up, so it needs the polled drain and the THRE toggle
# - all 600 "hello!" on COM1: no text lost when the ring is full
# qemu only: the X230 has no COM1 and leaves it off.
. "$(dirname "$0")/lib.sh"
Q=$1
[ -d "$Q" ] || { echo "usage: $0 <qemu dir>"; exit 1; }
T=10.1.54.222

ser=$HERE/test_com1_tx.log
(cd "$Q" && nohup "$HERE/qemu_ahci.sh" >qsq.out 2>&1 </dev/null &)
# qemu drops serial output until a client connects
socat -u TCP:127.0.0.1:4556,retry=100,interval=0.1 - >$ser 2>/dev/null &
sleep 8
"$HERE/test_kq_burst.sh" $T | grep -E "PASS|FAIL|lines"
# wait for COM1 to go quiet, 115200 baud is slow
n0=-1; i=0
while [ $i -lt 60 ]; do
    n1=$(stat -c %s $ser)
    [ "$n1" = "$n0" ] && break
    n0=$n1; i=$((i + 3)); sleep 3
done
kill $(pgrep -f "^qemu-system-i386 -name sqvfs")
while pgrep -f "^qemu-system-i386 -name sqvfs" >/dev/null; do sleep 0.5; done
check "boot log on COM1" grep -a -q "^version" $ser
n=$(grep -a -o "hello!" $ser | wc -l)
echo "hello! on COM1: $n"
check "600 lines of output on COM1" [ "$n" = 600 ]
finish
