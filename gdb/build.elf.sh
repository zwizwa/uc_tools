#!/bin/bash
. $(dirname $0)/buildlib.sh

need_vars ARCH LD MAP E O A UC_TOOLS

ENV=$UC_TOOLS/gdb/env.$ARCH.sh
# echo "ENV=$ENV" >&2

. $ENV


$GCC $LDFLAGS -T$LD -Wl,-Map=$MAP -o $E $O $O_SYSTEM $A $LDLIBS
