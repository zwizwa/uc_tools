#!/bin/bash
HERE=$(dirname $0)
WASM=$(readlink -f $HERE/../wasm)
echo "serving $WASM" >&2
exec gdb \
     --eval-command="file $HERE/test_websocket_timeseries.dynamic.host.elf" \
     --eval-command="run $WASM /tmp/test.raw" \
     --eval-command="bt"
