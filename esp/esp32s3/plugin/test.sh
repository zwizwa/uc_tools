#!/bin/sh
set -xe
DEFAULT_IP=192.168.0.121
[ -z "$IP" ] && IP=$DEFAULT_IP
CMD_DIR=~/.result/synth_tools/linux
CMD="$CMD_DIR/tether_bl.dynamic.host.elf $IP"
export TETHER_BL_VERBOSE=1

if [ -z "$ADDR" ]; then
#ADDR=0x40389500
#ADDR=0x40390000
ADDR=0x3fca6584
fi

BIN=/tmp/test.bin

$CMD \
save_flash $ADDR 4 $BIN

hexdump -C $BIN


