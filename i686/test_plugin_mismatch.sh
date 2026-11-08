#!/bin/sh
# A plugin built for another kernel must not register, and must not
# take the kernel down.  The kernel checks the plugin header and logs
# the mismatch.
#   test_plugin_mismatch.sh <ip> <plugin built for another kernel>
# Versions are stamped to the minute: keep a sqlite_plugin.bin, touch
# i686/kernel.c, rebuild a minute later, and pass the old one here.
# Loading also unregisters the previous plugin first (boot_config.plugin
# is cleared before the slot is overwritten).  Log output goes to the
# console peer that sent the last line, so a console is kept open
# while the plugin loads.
. "$(dirname "$0")/lib.sh"
T=$1
[ -n "$2" ] || { echo "usage: $0 <ip> <other plugin>"; exit 1; }

push_kernel
push_plugin
check "matching plugin registers" sh -c 'echo "$0" | grep -a -q "^count"' "$(con sqtest)"
log=$HERE/test_plugin_mismatch.log
CON_DELAY=8 con "" >$log &
sleep 2
push_plugin "$2"
wait
check "kernel reports mismatch" grep -a -q "plugin built for kernel" $log
grep -a "plugin built" $log
out=$(CON_DELAY=2 con sqtest hello)
check "mismatched plugin refused" sh -c 'echo "$0" | grep -a -q "sqtest?"' "$out"
check "kernel still alive" sh -c 'echo "$0" | grep -a -q "hello!"' "$out"
finish
