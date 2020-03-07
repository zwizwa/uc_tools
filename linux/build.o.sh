[ -z "$UC_TOOLS" ] && UC_TOOLS=$(dirname $O)/..
[ -z "$GCC" ] && . $UC_TOOLS/linux/env.$ARCH.sh
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
