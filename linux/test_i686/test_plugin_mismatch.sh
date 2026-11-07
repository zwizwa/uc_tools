#!/bin/sh
# A plugin built for another kernel must not register, and must not
# take the kernel down.
#   test_plugin_mismatch.sh <ip> <plugin built for another kernel>
# Versions are stamped to the minute: keep a sqlite_plugin.bin, touch
# i686/kernel.c, rebuild a minute later, and pass the old one here.
# Loading also unregisters the previous plugin first (boot_config.plugin
# is cleared before the slot is overwritten).
. "$(dirname "$0")/lib.sh"
T=$1
[ -n "$2" ] || { echo "usage: $0 <ip> <other plugin>"; exit 1; }

push_kernel
push_plugin
check "matching plugin registers" sh -c 'echo "$0" | grep -a -q "^count"' "$(con sqtest)"
push_plugin "$2"
out=$(CON_DELAY=2 con sqtest hello)
check "mismatched plugin refused" sh -c 'echo "$0" | grep -a -q "sqtest?"' "$out"
check "kernel still alive" sh -c 'echo "$0" | grep -a -q "hello!"' "$out"
finish
