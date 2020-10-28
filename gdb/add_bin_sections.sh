#!/bin/bash

# FIXME: THIS IS NOT CORRECT!

# The loader looks at segments, not at sections.
# And we get a warning:
# arm-none-eabi-objcopy: studBdak: warning: allocated section `.part_a' not in segment

# It might work with gdb, but I've run into this before and there are
# issues that I do not remember...
#
# For now I'm using elf->bin and then dd to load a binary at a specific address.


set -e
# set -x

# Add binary sections to an elf file.  This can be used to add a
# loader to an application, or an appliaction payload to a loader,
# depending on which symbols should be preserved for debugging.

[ -z "$2" ] && echo "usage: $0 <in> <out> [<addr> <section_name> <file.bin>] ... " && exit 1
in="$1"
out="$2"
cp -a "$in" "$out"
shift
shift
while [ ! -z "$1" ]; do
    addr="$1"
    section="$2"
    bin="$3"
    [ -z "$bin" ] && echo "missing filename" && exit 1
    echo $addr $section $bin
    arm-none-eabi-objcopy \
        --add-section $section=$bin \
        --change-section-address $section=$addr \
        --set-section-flags $section=alloc,load,data \
        "$out"
    shift
    shift
    shift
done

# arm-none-eabi-objdump -h "$out"

