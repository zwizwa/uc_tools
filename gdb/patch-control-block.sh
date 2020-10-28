#!/bin/bash

# This is hopefully the end of a long period of suffering: how to add
# checksums to ELF files?

# Previous approaches had bad interactions and were very complex and
# ad-hoc, producing a lot of useless intermediate products.  I'm now
# switching to an approach where there is alwas a .control section in
# the ELF which contains metadata such as CRC that can only be
# computed after the file has been generated.

# Summary:
#
# 1. What we do: produce an .elf file with 2-step patched CRC.
#
# 2. Do it in-place and keep it idempotent.  That makes it much easier
#    to hide everything inside build.sh
#
# 3. Keep all intermediate files hidden, e.g. don't express it as a
#    Makefile rule, it is too confusing.  Use only this script + the
#    bin2fw.c program that computes the control block.  One would
#    think that this is what Makefiles are for, but really, they suck.
#    And it's ok.  This patching is a simple step that can just run as
#    part of the linking step.
#
# 4. If raw binaries are needed, they can be extracted from this
#    patched .elf file.



# So what we do here:
#
# - Take the original ELF, produced by an .ld file that reserves space
#   for the firmware control block, e.g. empty .control section.
#
# - Convert it to BIN, but do not expose this intermediate file to
#   avoid confusion.
#
# - Use fw2bin to produce the firmware control block.  This operation
#   is idempotent.
#
# - Use --update-section to replace the control block in the ELF.

set -e

[ -z "$1" ] && echo "usage: $0 <elf>" && exit
ELF="$1"

# Caller is responsible for building this binary.
[ -z "$BIN2FW" ] && BIN2FW=$(dirname $0)/../linux/bin2fw.dynamic.host.elf
[ ! -x "$BIN2FW" ] && echo "BIN2FW=$BINFW is not executable" && exit 1

# To keep it clean, this should be configured explicitly.  However we
# only ever do this for arm-none-eabi, so use a default for now.
[ -z "$OBJCOPY" ] && OBJCOPY=arm-none-eabi-objcopy

# Assume it is ok to write intermediates next to the original.
BIN="$ELF.bin.tmp"
FW="$ELF.fw.tmp"
CONTROL="$ELF.control.tmp"

cleanup() {
    rm -f "$BIN" "$FW" "$CONTROL"
}

cleanup

"$OBJCOPY" -O binary "$ELF" "$BIN"
"$BIN2FW" "$BIN" "$FW" "$CONTROL"

hd "$CONTROL"

"$OBJCOPY" --update-section .control="$CONTROL" "$ELF"

cleanup



