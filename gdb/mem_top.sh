#!/bin/bash

[ -z "$1" ] && echo "usage: $0 <elf>" && exit 1

[ ! -f "$1" ] && echo "$1 not found" && exit 1

. $(dirname $0)/env.f103.sh

# _ebss was already marked.  The _eflash marker has been added in the
# linker script that produced the .elf

EBSS=$($OBJDUMP -x $1 | grep '_ebss$' | awk '{print $1}')
EFLASH=$($OBJDUMP -x $1 | grep '_eflash$' | awk '{print $1}')

# echo "EBSS=$EBSS" >&2
# echo "EFLASH=$EFLASH" >&2

cat <<EOF
MEMORY {
        rom (rx)  : ORIGIN = 0x${EFLASH}, LENGTH = 0xFFFFFFFF
        ram (rwx) : ORIGIN = 0x${EBSS}, LENGTH = 0xFFFFFFFF
}
EOF
