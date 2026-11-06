#!/bin/sh
# Set boot_config.ip in a kernel.dsk/.nbp/.3if: 512 + offsetof(ip)
# usage: set-ip.sh <in> <out> <a.b.c.d>
cp "$1" "$2"
printf "$(printf '\\%03o' $(echo $3 | tr . ' '))" |
    dd of="$2" bs=1 seek=524 conv=notrunc 2>/dev/null
