#!/bin/sh
[ -z "$IP" ] && IP=192.168.6.195 # FIXME
exec /i/tom/rdm-bridge/tools/tether_bl.dynamic.host.elf $IP "$@"

