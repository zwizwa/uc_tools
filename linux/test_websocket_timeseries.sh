#!/bin/bash
HERE=$(dirname $0)
WASM=$(readlink -f $HERE/../wasm)
echo "serving $WASM" >&2
exec socat TCP-LISTEN:3456,reuseaddr,fork EXEC:"$HERE/test_websocket.dynamic.host.elf $WASM"
