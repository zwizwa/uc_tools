#!/bin/bash
[ -z "$1" ] && echo "usage: $0 <port> [<host>]" && exit 1
export CONNECT_PORT=$(basename "$1")
export CONNECT_HOST=$(basename "$2")
[ -z "$CONNECT_HOST" ] && CONNECT_HOST=localhost
# FIXME: EXEC target doesn't yet support arguments, so use connect.sh wrapper
exec $(dirname $0)/packet_bridge_main.dynamic.host.elf HEX EXEC:4:$(dirname $0)/connect.sh
