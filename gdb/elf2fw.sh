#!/bin/sh

# This is hopefully the end of a long winding road: how to add
# checksums to ELF files without creating a huge mess?

# Previous approaches had a lot of accidental complexity.  I'm now
# settling on the following ideas:
#
# 1. Do not produce raw binaries in the build system that are then
#    used by other build rules.  You almost always want ELF to be the
#    main format, and when a raw binary is needed, simply project it
#    down to a raw binary using "objcopy -O binary".  It is _much_
#    simpler to generate intermediate binaries when they are necessary
#    in a script such as this one, and then delete them and
#    re-generate them if needed elsewhere.  The operation is fast and
#    it is really not worth the trouble of cluttering the build with
#    files.
#
# 2. Accept that adding a checksum to an ELF is necessarily a 2-step
#    process.  DO NOT USE IN PLACE PATCHING FOR THIS.  It is too hard
#    to see whether a patch has happened or not.  We use the .fw
#    extension to indicate that the file is in ELF format, and that it
#    has been processed to add the ".control" block with CRC.
#
# 3. It is simplest to let the linker reserve a section, and then
#    patch it later with "objcopy --modify-section".  The
#    --add-section option does not update the segment table which
#    leads to improperly structured ELF files.  Support for this has
#    been added to the link script.
#



# Practocally, what we do here:
#
# - Take the original ELF, produced by an .ld file that reserves space
#   for the firmware control block, e.g. empty .control section.
#
# - Convert it to BIN, but do not expose this intermediate file.  It
#   will be deleted before the end of the script.
#
# - Use fw2bin to produce the firmware control block.  This operation
#   is idempotent.
#
# - Copy .elf to .fw.tmp
#
# - Use --update-section on the .fw to replace the control block in
#   the ELF.
#
# - Bless the file by renaming .fw.tmp to .fw

set -e
set -x

[ -z "$1" ] && echo "usage: $0 <elf> [<fw>]" && exit
ELF="$1"
FW="$2"
[ -z "$FW" ] && FW=$(basename "$ELF" .elf).fw.elf

# Avoid false positives in case script fails
rm -f "$FW"

# Caller is responsible for building this binary from bin2fw.c and is
# allowed to specify its location.  Otherwise we use default.
[ -z "$BIN2FW" ] && BIN2FW=$(dirname $0)/../linux/bin2fw.dynamic.host.elf
[ ! -x "$BIN2FW" ] && echo "BIN2FW=$BINFW is not executable" && exit 1

# To keep it clean, this should be configured explicitly.  However we
# only ever do this for arm-none-eabi, so use a default for now.
[ -z "$OBJCOPY" ] && OBJCOPY=arm-none-eabi-objcopy

# Assume it is ok to write intermediates next to the original.
BIN="$ELF.bin.tmp"
FW_BIN="$ELF.fw.bin.tmp"
FW_TMP="$FW.tmp"
CONTROL="$ELF.control.bin.tmp"
SHA1_FILE="$FW.sha1.tmp"

echo "ELF=$ELF FW=$FW"

cleanup() {
    rm -f "$BIN" "$FW_BIN" "$CONTROL" "$FW_TMP" "$SHA1_FILE"
}

cleanup

"$OBJCOPY" -O binary "$ELF" "$BIN"
"$BIN2FW" "$BIN" "$FW_BIN" "$CONTROL" "$SHA1_FILE"

hexdump -C "$CONTROL" >&2
SHA1=$(cat $SHA1_FILE)
echo "SHA1=$SHA1" >&2

cp -a "$ELF" "$FW_TMP"

# Update the control section.
"$OBJCOPY" --update-section .control="$CONTROL" "$FW_TMP"
mv "$FW_TMP" "$FW"



cleanup

echo "FW=$FW" >&2
ls -l "$FW" >&2


# The control block that is added into the new elf contains the hash
# of the original elf.  The ELF_SHA1_DIR points to a content
# addressable store (CAS) where we add the file together with a gc
# root.  Do this at the end such that we do not save when firmware
# conversion failed.

# FIXME: Store needs to be parameterized by hash.

if [ ! -z "$ELF_CAS" ]; then
    $ELF_CAS/.m.store $FW $(basename $FW) $SHA1
fi





