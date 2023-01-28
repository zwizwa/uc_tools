#!/bin/sh
HERE=$(readlink -f $(dirname "$0"))

# exec socat TCP-LISTEN:20000 EXEC:$HERE/gdbstub_proxy.dynamic.host.elf

# Supports TCP directly now
gdb \
    -quiet \
    -ex run \
    --args \
    $HERE/gdbstub_proxy.dynamic.hostdbg.elf \
    /home/tom/rdm-bridge/coredump/ram.ttyACM1.20230127-175441.bin \



