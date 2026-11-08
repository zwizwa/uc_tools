#!/bin/sh
# LOG from an isr goes through the kernel event queue as
# KERNEL_EVENT_LOG and is printed by the main loop.
#   test_isr_log.sh <ip>
# A garbage packet on the 3if port fails the crc check, and udp_mon
# logs "udp_mon bad crc" from the net isr.  Log output goes to the
# console peer that sent the last line, so a console is kept open.
. "$(dirname "$0")/lib.sh"
T=$1
[ -n "$T" ] || { echo "usage: $0 <ip>"; exit 1; }

log=$HERE/test_isr_log.log
CON_DELAY=4 con "" >$log &
sleep 2
printf 'garbage' | socat -u - UDP-SENDTO:$T:799
wait
grep -a "bad crc" $log
check "isr LOG printed by the main loop" grep -a -q "udp_mon bad crc" $log
check "kernel still alive" sh -c 'echo "$0" | grep -a -q "hello!"' "$(CON_DELAY=2 con hello)"
finish
