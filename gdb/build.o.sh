need() {
    for var in $@; do
        [ -z $(eval "echo \$$var") ] && echo "$var is undefined" && exit 1
    done
}

need ARCH FIRMWARE O C D
[ -z "$VERSION"  ] && VERSION=current
[ -z "$UC_TOOLS" ] && UC_TOOLS=$(dirname $O)/..

. $UC_TOOLS/gdb/env.$ARCH.sh
$GCC \
    $CFLAGS \
    $CFLAGS_EXTRA \
    -I$UC_TOOLS_GDB_DIR \
    -I$UC_TOOLS_GDB_DIR/.. \
    -MD -MF $D \
    -DFIRMWARE=\"$FIRMWARE\" \
    -DBUILD=\"$VERSION\" \
    -o $O \
    -c $C
