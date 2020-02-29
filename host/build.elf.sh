UC_TOOLS=$(dirname $E)/..
. $UC_TOOLS/host/env.$ARCH.sh
# This does not use a linker script.
$GCC $LDFLAGS -Wl,-Map=$MAP -o $E $O $O_SYSTEM $A $LDLIBS
