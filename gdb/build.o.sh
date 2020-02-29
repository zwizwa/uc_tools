UC_TOOLS=$(dirname $O)/..
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
