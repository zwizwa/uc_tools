#!/bin/bash

set -e
# set -x

# Add binary sections to an elf file, in-place.
[ -z "$2" ] && echo "usage: $0 <in> <out> [<addr> <file.bin>] ... " && exit 1
in="$1"
out="$2"
cp -a "$in" "$out"
shift
shift
while [ ! -z "$1" ]; do
    addr="$1"
    bin="$2"
    [ -z "$bin" ] && echo "missing filename" && exit 1
    echo $addr $bin
    section=".$bin"
    # Re-use the filename as section name.
    arm-none-eabi-objcopy \
        --add-section $section=$bin \
        --change-section-address $section=$addr \
        --set-section-flags $section=alloc,load,data \
        "$out"
    shift
    shift
done

arm-none-eabi-objdump -h "$out"

# CONTENTS, ALLOC, LOAD, DATA
# CONTENTS, READONLY
