#!/bin/sh
# rtl8139 survives rx overflow: UDP flood, then it still answers.
#   test_rtl8139_flood.sh <ip>
# In qemu the emulated card outruns the kernel and rxovw counts up,
# a real card at 100M did not overflow (rxovw=0).  The old driver
# treated RXOVW as fatal and only toggled RE, so CBR kept its place
# and rx stayed out of sync for good.
. "$(dirname "$0")/lib.sh"
T=$1
[ -n "$T" ] || { echo "usage: $0 <ip>"; exit 1; }

for i in 1 2 3 4; do
    timeout 3 socat -u -b 1472 /dev/zero UDP-SENDTO:$T:9 2>/dev/null &
done
wait
sleep 1
check "answers ping after flood" sh -c "ping -c3 -W1 $T | grep -q ' 0% packet loss'"
CON_DELAY=2 con netstat | grep -a -E "rxovw|CBR"
finish
