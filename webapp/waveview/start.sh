#!/bin/bash

# Start http listener that serves files in this directory.
# Start reading at index.html

ELF=http_waveview.dynamic.host.elf

# FIXME: restarting from Erlang doesn't kill child.
killall $ELF

HERE=$(dirname $0)
webroot=$(readlink -f $HERE)
echo "serving $webroot" >&2
exec gdb 2>&1 \
     --quiet \
     --eval-command="file $HERE/../../linux/$ELF" \
     --eval-command="run $webroot /home/tom/TEST/test.raw" \
     --eval-command="bt"
