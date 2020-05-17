[ -z "$UC_TOOLS" ] && UC_TOOLS=$(dirname $O)/..

assert_vars() {
    for var in $@; do
        # echo "checking $var=\"$(eval "echo \$$var")\"" >&2
        if [ -z "$(eval "echo \$$var")" ]; then
             echo "$var is undefined" >&2
             exit 1
        fi
    done
}

assert_vars TYPE


case "$TYPE" in
    o)
        assert_vars ARCH
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
        ;;
    a)
        assert_vars A OBJECTS
        ar -r $A $OBJECTS #2>/dev/null
        ;;
    ld)
        # This is not a real file for linux builds.
        ;;
    elf)
        assert_vars LD ARCH MAP E A
        [ -z "$GCC" ] && . $UC_TOOLS/linux/env.$ARCH.sh
        # The LD name is fake. Use linker's defaults.
        if [ $(basename "$LD") != dynamic.$ARCH.ld ]; then
            echo "Only dynamic linking: ARCH=$ARCH LD=$LD"
            exit 1
        fi
        $GCC $LDFLAGS -Wl,-Map=$MAP -o $E $O $O_SYSTEM $A $LDLIBS
        ;;
    *)
        echo "TYPE=$TYPE unknown" >&2
        exit 1
        ;;
esac

