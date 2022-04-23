#!/bin/sh
set -e
# set -x
[ -z "$2" ] && echo "usage: $0 <out.hex> <in1.hex> [<in2.hex> ...]" && exit 1
OUT="$1"
TMP="$OUT.tmp"
shift
rm -f "$OUT" "$TMP"
# echo "TMP=$TMP" >&2

# Strip the End Of File record from each file, and add one at the end.
# https://en.wikipedia.org/wiki/Intel_HEX
cat "$@" | grep -v '^:00000001FF' >"$OUT.tmp"
cat "$1" | grep    '^:00000001FF' >>"$OUT.tmp"

mv "$OUT.tmp" "$OUT"
