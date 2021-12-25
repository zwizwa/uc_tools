#!/bin/bash
# See other start.sh scripts
ELF=http_wasm.dynamic.host.elf
killall $ELF
HERE=$(dirname $0)
webroot=$(readlink -f $HERE)
echo "serving $webroot" >&2
exec gdb 2>&1 \
     --quiet \
     --eval-command="file $HERE/../../linux/$ELF" \
     --eval-command="run $webroot" \
     --eval-command="bt"
