#!/bin/sh
[ -z "$UC_TOOLS" ] && UC_TOOLS=$(dirname $O)/..

. $(dirname $0)/../build_lib.sh



CLOSURE_VARS="
O C D A ARCH FIRMWARE DATA LD_GEN LD MAP ELF BIN DASM HEX BIN2FW ADDR
UC_TOOLS TYPE GCC CFLAGS CFLAGS_EXTRA UC_TOOLS_GDB_DIR FW
VERSION_LINK VERSION_LINK_GEN ELF_SHA1_DIR
"

# set -x

assert_vars TYPE

case "$TYPE" in
    o)
        assert_vars ARCH O
        dump_closure_to_file ${O}.build
        rm -rf "$O"

        [ -z "$GCC" ] && . $UC_TOOLS/linux/env.$ARCH.sh

        # set -x

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
        dump_closure_to_file ${A}.build
        rm -f "$A"
        ar -r $A $OBJECTS #2>/dev/null
        ;;
    ld)
        # This is not a real file for linux builds.
        ;;
    elf)
        assert_vars LD ARCH MAP ELF A
        dump_closure_to_file ${ELF}.build
        rm -f "$ELF"

        [ -z "$GCC" ] && . $UC_TOOLS/linux/env.$ARCH.sh
        # For linux applications we do not use linker scripts.
        # Dynamic linking is the default.  FIXME: This no longer
        # insists on the "dynamic" name, such that link type can still
        # be used as a tag to e.g. provide different linker parameters
        # in the build system.
        # if [ $(basename "$LD") != dynamic.$ARCH.ld ]; then
        #     echo "Only dynamic linking: ARCH=$ARCH LD=$LD"
        #     exit 1
        # fi
        $GCC $LDFLAGS -Wl,-Map=$MAP -o $ELF $O $O_SYSTEM $A $LDLIBS
        ;;
    so)
        assert_vars LD ARCH MAP SO A
        dump_closure_to_file ${SO}.build
        rm -f "$SO"

        [ -z "$GCC" ] && . $UC_TOOLS/linux/env.$ARCH.sh
        # The LD name is fake. Use linker's defaults.
        if [ $(basename "$LD") != dynamic.$ARCH.ld ]; then
            echo "Only dynamic linking: ARCH=$ARCH LD=$LD"
            exit 1
        fi
        set -x
        $GCC $LDFLAGS -Wl,-Map=$MAP -o $SO $O $O_SYSTEM $A $LDLIBS -shared
        ;;
    *)
        echo "TYPE=$TYPE unknown" >&2
        exit 1
        ;;
esac

