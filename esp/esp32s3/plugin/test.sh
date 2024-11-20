#!/bin/sh
set -xe
DEFAULT_IP=192.168.0.121
[ -z "$IP" ] && IP=$DEFAULT_IP
CMD_DIR=~/.result/synth_tools/linux
CMD="$CMD_DIR/tether_bl.dynamic.host.elf $IP"
export TETHER_BL_VERBOSE=1


$CMD \
save_flash 0x40389500 4 /tmp/flash.bin


