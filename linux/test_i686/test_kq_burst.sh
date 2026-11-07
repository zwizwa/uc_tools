#!/bin/sh
# Kernel event queue: 600 console lines in packets of 10, padded to
# 5..95 bytes so the 4K ring wraps several times.  Every line must be
# delivered or counted as dropped, see kqstat.
#   test_kq_burst.sh <ip>
# Expect drops=0 on the X230.  In qemu COM1 output busy-waits at
# emulated 115200 baud inside the isr (telnet echo), which starves the
# main loop: drops > 0 there, but events + drops still add up.  Use
# ALLOW_DROPS=1 to skip that check.
. "$(dirname "$0")/lib.sh"
T=$1
[ -n "$T" ] || { echo "usage: $0 <ip>"; exit 1; }

# Prints events, drops, tries.  Retries: after the burst qemu can be
# busy for a while.  Each try is itself an event or a drop.
kq() {
    for i in 1 2 3 4 5 6 7 8 9 10; do
        r=$(CON_DELAY=2 con kqstat | sed -n 's/^kq events=\([0-9]*\) drops=\([0-9]*\).*/\1 \2/p')
        [ -n "$r" ] && { echo $r $i; return; }
        sleep 3
    done
}
lines() {
    p=0
    while [ $p -lt 60 ]; do
        i=0; out=""
        while [ $i -lt 10 ]; do
            out="${out}hello$(printf "%$(( (p * 10 + i) * 7 % 90 ))s" "")
"
            i=$((i + 1))
        done
        printf "%s" "$out"
        sleep 0.05
        p=$((p + 1))
    done
    sleep 2
}

set -- $(kq); e0=$1; d0=$2
lines | timeout 15 socat -b 2048 - UDP:$T:1001 >/dev/null 2>&1
sleep 3
set -- $(kq); e1=$1; d1=$2; tries=$3
n=$((e1 - e0 + d1 - d0 - tries))
echo "events $e0 -> $e1, drops $d0 -> $d1"
check "600 lines delivered or counted" [ $n = 600 ]
[ "$ALLOW_DROPS" = 1 ] || check "no drops" [ "$d1" = "$d0" ]
finish
