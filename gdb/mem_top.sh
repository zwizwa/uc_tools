#!/bin/bash

[ -z "$1" ] && echo "usage: $0 <elf>" && exit 1

[ ! -f "$1" ] && echo "$1 not found" && exit 1

. $(dirname $0)/env.f103.sh

# The symbol _ebss was already marked in the linker script.  
EBSS="0x$($OBJDUMP -x $1 | grep '_ebss$' | awk '{print $1}')"

# Similarly, _eflash marks the end of flash memory.
EFLASH_HEX=$($OBJDUMP -x $1 | grep '_eflash$' | awk '{print $1}')

# Round that up to page aligment
ALIGN=1024
EFLASH_ALIGN_DEC=$(((((0x$EFLASH_HEX - 1)/$ALIGN)+1)*$ALIGN))
EFLASH_ALIGN=$(printf "0x%08X\n" $EFLASH_ALIGN_DEC)

# echo "EBSS=$EBSS" >&2
# echo "EFLASH=$EFLASH" >&2

cat <<EOF
MEMORY {
        rom (rx)  : ORIGIN = ${EFLASH_ALIGN}, LENGTH = 0xFFFFFFFF
        ram (rwx) : ORIGIN = ${EBSS}, LENGTH = 0xFFFFFFFF
}
EOF
