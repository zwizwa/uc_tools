need() {
    for var in $@; do
        [ -z $(eval "echo \$$var") ] && echo "$var is undefined" && exit 1
    done
}

need ARCH LD MAP E O A
[ -z "$UC_TOOLS" ] && UC_TOOLS=$(dirname $E)/..

. $UC_TOOLS/gdb/env.$ARCH.sh
set -x
$GCC $LDFLAGS -T$LD -Wl,-Map=$MAP -o $E $O $O_SYSTEM $A $LDLIBS
