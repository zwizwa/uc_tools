#!/bin/bash

# TL,DR: Build system is split into two parts:
# 1. Build tool wrappers (this file)
# 2. Build rules (either apenwarr redo, or exo redo)


# The build system is a little complex as it is constructed to support
# a number of use cases and is still in flux.  Some history:
#
# 1. This Started out as Makefiles, but those were deemed to be too
# cumbersome to use due to the well known "recursive make" issues.
#
# 2. I discovered apenwarr's redo, which solved the recursive make
# issues, but landed me in horrible shell programming to be able to
# support different target and link types in the same build system.
#
# 3. I wrote a version of redo in Erlang, which was able to provide
# the expressivity needed to implement the rules in a sane way, but
# this landed me in non-standard territory.


# So what is the design I ended up with?
#
# - 1. Keep separate build.*.sh files that take environment variables
#      and perform a build.
#
# - 2. Keep the default.*.do files to call into those build.*.sh
#      files, to allow people to either use apenwarr redo, or minimal
#      do.
#
# - 3. Use the Erlang system during development.


# Eventually, I would like to get rid of apenwarr redo, and generate
# Makefiles from my Erlang redo.


# Some design rules for the build.*.sh scripts
#
# - These should be standalone executable scripts, not "dot scripts".
#
# - All used variables should be checked with the need() function to
#   show proper error messages on misconfiguration.

assert_vars() {
    for var in $@; do
        # echo "checking $var=\"$(eval "echo \$$var")\"" >&2
        if [ -z "$(eval "echo \$$var")" ]; then
             echo "$var is undefined" >&2
             exit 1
        fi
    done
}



assert_vars UC_TOOLS
[ -z "$VERSION" ] && VERSION=current

case "$TYPE" in
    o)
        assert_vars O C D ARCH FIRMWARE
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
        ;;
    o_data)
        assert_vars O DATA ARCH 
        . $UC_TOOLS/gdb/env.$ARCH.sh
        assert_vars ELFTYPE BINARCH
        $OBJCOPY \
            --input-target=binary \
            --output-target=$ELFTYPE \
            --binary-architecture=$BINARCH \
            "$DATA" "$O"
        ;;
    a)
        # Objects is allowed to be empty for empty stub libs
        assert_vars A
        ar -r $A $OBJECTS 2>/dev/null
        ;;
    ld)
        assert_vars LD_GEN LD
        $LD_GEN >$LD
        ;;
    elf)
        assert_vars ARCH LD MAP ELF O A
        . $UC_TOOLS/gdb/env.$ARCH.sh
        # Optionally link to parent elf file
        [ ! -z "$PARENT_ELF" ] && PARENT_ELF_LDFLAGS=-Wl,--just-symbols=$PARENT_ELF
        $GCC \
            $LDFLAGS \
            -T$LD \
            -Wl,-Map=$MAP \
            $PARENT_ELF_LDFLAGS \
            -o $ELF \
            $O $O_SYSTEM $A $LDLIBS
        ;;
    bin)
        assert_vars ARCH ELF BIN
        . $UC_TOOLS/gdb/env.$ARCH.sh
        # $OBJDUMP -d $ELF
        $OBJCOPY -O binary $ELF $BIN
        ;;

    *)
        echo "unknown TYPE=$TYPE"
        exit 1
        ;;
esac
