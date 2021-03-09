#!/bin/bash
DIR=$(dirname $0)
exec socat TCP-LISTEN:3456,reuseaddr,fork EXEC:"$DIR/test_websocket.dynamic.host.elf $DIR/../web"
