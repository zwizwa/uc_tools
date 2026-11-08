#!/bin/sh
# PIT timer and the main loop -> isr command queue, through the
# test_timer plugin (i686/test_timer_plugin.c).
#   test_timer.sh <ip>
# - ttest:  10 alarms 50ms apart, in order, at most 2ms late
# - tflood: 200 alarms, more than the command queue (1K) and alarm
#   table (16) hold: posting fails, KERNEL_EVENT_ROOM resumes it, all
#   200 fire in order
# Results are logged asynchronously: a console is kept open.
. "$(dirname "$0")/lib.sh"
T=$1
[ -n "$T" ] || { echo "usage: $0 <ip>"; exit 1; }

push_kernel
push_plugin "$I686/test_timer_plugin.bin"
out=$(CON_DELAY=3 con ttest tflood)
echo "$out" | grep -a "timer test"
check "ttest" sh -c 'echo "$0" | grep -a -q "timer test ok n=10 "' "$out"
check "tflood" sh -c 'echo "$0" | grep -a -q "timer test ok n=200 "' "$out"
check "tflood needed ROOM" sh -c 'echo "$0" | grep -a "timer test ok n=200 " | grep -a -q -v "rooms=0 "' "$out"
finish
