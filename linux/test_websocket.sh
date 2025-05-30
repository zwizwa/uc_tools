#!/bin/sh
HERE=$(dirname $0)
WASM=$(readlink -f $HERE/../webapp/wasm)
PORT=3456
echo "serving $WASM on port $PORT" >&2
exec socat TCP-LISTEN:$PORT,reuseaddr,fork EXEC:"$HERE/test_websocket.dynamic.host.elf $WASM"
