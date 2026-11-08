#!/bin/sh
# Load a plugin into a running kernel, show the console while it loads.
#   load_plugin.sh <ip> [<plugin.bin>]     default: <ip>'s in apps.csv
# KERNEL=1 pushes kernel.3if first.
# As root on psamathe.  The plugin must be built against the running
# kernel (version stamp), else the console says which one it wants.
# TETHER_UDP_GAP_US=2000 by default: the Soekris drops 3if bursts.
HERE=$(cd "$(dirname "$0")" && pwd)
G=$HERE/glibc
[ -d "$G" ] && TETHER=${TETHER:-"$G/ld-linux-x86-64.so.2 --library-path $G $HERE/../linux/tether_udp.dynamic.host.elf"}
. "$HERE/lib.sh"
T=$1
[ -n "$T" ] || { echo "usage: $0 <ip> [<plugin.bin>]"; exit 1; }
export TETHER_UDP_GAP_US=${TETHER_UDP_GAP_US:-2000}
A=$(awk -F, -v ip="$T" '$1 == ip { print $2 }' "$HERE/apps.csv")
P=${2:-${A:+$I686/$A}}
[ -n "$KERNEL" ] && push_kernel
[ -n "$P" ] || { echo "$T: no plugin in apps.csv"; exit 1; }

CON_DELAY=6 con "" | grep -a -v "^\[IDLE\]" &
sleep 1
tu plugin "$P"
wait
