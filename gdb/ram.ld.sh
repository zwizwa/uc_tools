#!/bin/bash

# FIXME: There are currently no linker scripts that handle this in any
# automatic way.  There are too many degrees of freedom which
# eventually end up in wanting a dynamic linker that can dynamically
# reload.  Just use the Flash, but develop code from the gdbstub to
# avoid slow reloads.

#----------------------------------------------------------------------

# Generate an .ld config file that can be used to load a program in to
# microcontroller RAM. 

# Use the end of BSS as the ORIGIN of the code.  The LENGTH is set to
# a dummy value.  It is the responsibility of the programmer to not
# blow the stack, which resides between _ebss and the top of memory.

[ -z "$1" ] && echo "usage: $0 <elf>" && exit 1

[ ! -f "$1" ] && echo "$1 not found" && exit 1

. $(dirname $0)/env.f103.sh

EBSS=$($OBJDUMP -x $1 | grep _ebss | awk '{print $1}')


cat <<EOF
MEMORY {
        ram (rwx) : ORIGIN = 0x${EBSS}, LENGTH = 0xFFFFFFFF
}
INCLUDE ram.ld
EOF
