#!/bin/bash
if [ "$1" != "start" ]; then
    echo "FIXME: Non-daemon mode is not implemented for $0.  Use \"$0 start\" instead." && exit 0
fi



ELF=test_ws.dynamic.host.elf

# FIXME: restarting from Erlang doesn't kill child.
killall $ELF

HERE=$(dirname $0)
webroot=$(readlink -f $HERE/../webroot)
echo "serving $webroot" >&2
exec gdb 2>&1 \
     --quiet \
     --eval-command="file $HERE/$ELF" \
     --eval-command="run $webroot /home/tom/test.raw" \
     --eval-command="bt"
