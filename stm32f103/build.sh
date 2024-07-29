#!/bin/sh

# TL,DR: Build system is split into two parts:
# 1. Build tool wrappers (this file)
# 2. Build rules (either apenwarr redo, or exo redo, or Makefile)


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

set -e
# set -x

. $(dirname $0)/../build_lib.sh



CLOSURE_VARS="
O C D A ARCH FIRMWARE DATA LD_GEN LD MAP ELF BIN DASM HEX BIN2FW ADDR
UC_TOOLS TYPE GCC CFLAGS CFLAGS_EXTRA CFLAGS_OPTI UC_TOOLS_GDB_DIR FW
VERSION_LINK VERSION_LINK_GEN ELF_SHA1_DIR
"

# FIXME: Dump it next to output instead.
# dump_closure_default




# A note about $VERSION

# - If defined, we will propagate $VERSION to the string macro BUILD
#   which is picked up by the .o build rule, so it can be embedded in
#   binaries.
#
# - If the env var is not defined, we will not define the macro.
#
# - This can be used to support two build mechanisms:
#
#   - Clean reelease build, where VERSION is clearly defined, and each
#     .o file build picks up the environment variable.
#
#   - Development build, where VERSION is _not_ defined, and a
#     #ifndef VERSION is used to include a header file that was
#     generated just before running the normal build process, such that
#     the change of this file acts as a proper dependency, and
#     retriggers the building of the .o file that contains the version
#     string.

assert_vars UC_TOOLS

case "$TYPE" in
    o)
        assert_vars O C D ARCH FIRMWARE
        dump_closure_to_file ${O}.build
        # Note that CFLAGS_OPTI is included into CFLAGS here:
        . $UC_TOOLS/stm32f103/env.$ARCH.sh
        # echo "GCC=$GCC ($(readlink -f $GCC))"
        # $GCC --version
        [ ! -z "$VERSION" ] && DEFINE_BUILD_VERSION="-DBUILD=\"$VERSION\""
        # echo "CFLAGS=$CFLAGS" >&2
        $GCC \
            $CFLAGS \
            $CFLAGS_EXTRA \
            -I$UC_TOOLS_GDB_DIR \
            -I$UC_TOOLS_GDB_DIR/.. \
            -MD -MF $D \
            -DFIRMWARE=\"$FIRMWARE\" \
            $DEFINE_BUILD_VERSION \
            -o $O \
            -c $C
        ;;

    o_data)
        assert_vars O DATA ARCH 
        dump_closure_to_file ${O}.build
        . $UC_TOOLS/stm32f103/env.$ARCH.sh
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
        dump_closure_to_file ${A}.build
        ar -r $A $OBJECTS 2>/dev/null
        #|| echo "ERROR: ar" >&2
        ;;
    ld)
        assert_vars LD_GEN LD
        dump_closure_to_file ${LD}.build
        $LD_GEN >$LD
        ;;
    elf)
        assert_vars ARCH LD MAP ELF O A
        dump_closure_to_file ${ELF}.build
        . $UC_TOOLS/stm32f103/env.$ARCH.sh
        # About versioning: I want incremental builds, and incremental
        # uploads, and I want to _not_ upload when nothing changed.
        # This makes it difficult to include a generated version file
        # in the dependencies of the elf file (did something change,
        # or did just the version change?).  Instead it is easier to
        # think of version tagging as something that happens at link
        # time.  And re-linking only happens when an actual dependency
        # changed.
        #
        # Allow it to be a generator.  This is convenient as a default.
        if [ -z "$VERSION_LINK" ] && [ ! -z "$VERSION_LINK_GEN" ]; then
            VERSION_LINK=$($VERSION_LINK_GEN)
        fi

        if [ ! -z "$VERSION_LINK" ]; then
            C_VERSION_LINK="$ELF.version_link.c"
            O_VERSION_LINK="$ELF.version_link.o"
            # FIXME: hardcoded section.  make that configurable.
            echo "const char config_version[] __attribute__ ((section (\".config_data\"))) = \"$VERSION_LINK\";" >$C_VERSION_LINK
            $GCC -o "$O_VERSION_LINK" -c "$C_VERSION_LINK" || exit 1
        fi
        echo "$VERSION_LINK $ELF" >&2

        # Optionally link to parent elf file
        [ ! -z "$PARENT_ELF" ] && PARENT_ELF_LDFLAGS=-Wl,--just-symbols=$PARENT_ELF
        $GCC \
            $LDFLAGS \
            -T$LD \
            -Wl,-Map=$MAP \
            $PARENT_ELF_LDFLAGS \
            -o $ELF \
            $O $O_SYSTEM $O_VERSION_LINK $A $LDLIBS 

        # Remove the generated version_link.c file.  It gets in the
        # way for code searches.
        rm -f "$C_VERSION_LINK"
        ;;
    bin)
        # Convert an ELF file to .bin in the most straighforward way.
        # This will produce a file where the first byte is from the
        # loadable section that has the lowest address.  All the rest
        # is filled in and padded with zeros.
        assert_vars ARCH ELF BIN
        dump_closure_to_file ${BIN}.build
        . $UC_TOOLS/stm32f103/env.$ARCH.sh
        # $OBJDUMP -d $ELF
        $OBJCOPY -O binary $ELF $BIN
        ;;
    dasm)
        assert_vars ARCH ELF DASM
        dump_closure_to_file ${DASM}.build
        . $UC_TOOLS/stm32f103/env.$ARCH.sh
        rm -f $DASM
        # Just put the summary in this file as well.
        $OBJDUMP -x $ELF >$DASM
        $OBJDUMP -d $ELF >>$DASM
        ;;
    hex)
        assert_vars ARCH ELF HEX
        dump_closure_to_file ${HEX}.build
        . $UC_TOOLS/stm32f103/env.$ARCH.sh
        $OBJCOPY -O ihex $ELF $HEX
        ;;
    fw)
        # Note that this has changed.  We map ELF to ELF, instead of
        # BIN to BIN.  It is more trouble than it's worth to keep
        # intermediate raw binaries due to lack of metatdata.  If raw
        # binaries are necessary, derive them from ELF files.
        assert_vars ARCH ELF FW BIN2FW
        dump_closure_to_file ${FW}.build
        . $UC_TOOLS/stm32f103/env.$ARCH.sh
        # ls -l $BIN2FW
        # ls -l $UC_TOOLS/stm32f103/elf2fw.sh
        $UC_TOOLS/stm32f103/elf2fw.sh $ELF $FW $ELF_SHA1_DIR
        ;;
    enc)
        # Note that encryption key needs to be configured implicitly
        # in the FW2ENC encryptor wrapper.
        assert_vars ENC FW FW2ENC
        dump_closure_to_file ${ENC}.build
        $FW2ENC $FW $ENC
        ;;
    data)
        # Convert binary to elf to be loaded at address.  Note that
        # this is a last resort.  Of possible, please don't project
        # onto binary: use objcopy --modify-section to add data to an
        # ELF file after linking.
        assert_vars ARCH BIN DATA ADDR
        dump_closure_to_file ${DATA}.build
        . $UC_TOOLS/stm32f103/env.$ARCH.sh
        assert_vars ELFTYPE
        # $OBJDUMP -d $ELF
        $OBJCOPY -I binary -O $ELFTYPE --change-section-address .data=$ADDR $BIN $DATA
        ;;
    *)
        echo "unknown TYPE=$TYPE"
        exit 1
        ;;
esac
