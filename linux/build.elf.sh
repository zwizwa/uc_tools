[ -z "$UC_TOOLS" ] && UC_TOOLS=$(dirname $E)/..
[ -z "$GCC" ] && . $UC_TOOLS/linux/env.$ARCH.sh

# The LD name is fake. Use linker's defaults.
if [ $(basename "$LD") != dynamic.$ARCH.ld ]; then
    echo "Only dynamic linking: ARCH=$ARCH LD=$LD"
    exit 1
fi
$GCC $LDFLAGS -Wl,-Map=$MAP -o $E $O $O_SYSTEM $A $LDLIBS
