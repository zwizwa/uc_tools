#!/bin/sh
# sqlite plugin on the type 7f partition, with crash recovery.
#   test_sqlite.sh <ip> [format]
# - pushes kernel.3if (sqcrash restarts from that image at 16MB)
# - loads sqlite_plugin.bin, optional sqformat (destroys the db)
# - sqtest twice: rows go up, integrity ok
# - sqcrash: crashes halfway through writing a 2000 row commit by
#   restarting the kernel, which drops the plugin
# - reload plugin, sqtest: hot journal found and rolled back
. "$(dirname "$0")/lib.sh"
T=$1
[ -n "$T" ] || { echo "usage: $0 <ip> [format]"; exit 1; }

push_kernel
push_plugin
[ "$2" = format ] && con sqformat
out=$(con sqtest sqtest)
echo "$out" | grep -a -E "^sqv|count|integ"
n1=$(echo "$out" | sed -n 's/^count(\*)=\([0-9]*\).*/\1/p' | head -1)
n2=$(echo "$out" | sed -n 's/^count(\*)=\([0-9]*\).*/\1/p' | tail -1)
check "sqtest adds a row" [ -n "$n1" -a "$n2" = "$((n1 + 1))" ]
check "integrity before crash" [ "$(echo "$out" | grep -a -c 'integrity_check=ok')" = 2 ]

out=$(CON_DELAY=8 con sqcrash)
check "sqcrash restarts" sh -c 'echo "$0" | grep -a -q "sqv crash"' "$out"
sleep 3
push_plugin
out=$(con sqtest)
echo "$out" | grep -a -E "^sqv|count|integ"
check "hot journal on open" sh -c 'echo "$0" | grep -a -q "journal=[1-9]"' "$out"
check "crashed commit rolled back" sh -c 'echo "$0" | grep -a -q "^count(\*)=$1 "' "$out" $((n2 + 1))
check "integrity after crash" sh -c 'echo "$0" | grep -a -q "integrity_check=ok"' "$out"
finish
