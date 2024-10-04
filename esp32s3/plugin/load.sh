#!/bin/sh
set -xe

# FIXME
# DEFAULT_IP=192.168.6.122
# DEFAULT_IP=192.168.0.122
DEFAULT_IP=192.168.1.35

[ -z "$IP" ] && IP=$DEFAULT_IP

# CMD_DIR="/i/tom/rdm-bridge/tools/"
CMD_DIR="/charon/home/tom/rdm-bridge/tools/"

CMD="$CMD_DIR/tether_bl.dynamic.host.elf $IP"



# Always relink.
HERE=$(dirname "$0")
cd $HERE
make link_clean

# Start command connection, get memory info, use that to link the
# firmware while we have the connection open and upload the bin files.
#
# Note the terminology here is confusing because 3if only knows about
# ram and flash (where f is sometimes interpreted as "fussy" memory).
# But flash referes to IRAM and ram refers to DRAM here.

$CMD \
write_meminfo meminfo.sh \
system make \
load_meminfo_flash ./test.iram.bin \
load_meminfo_ram   ./test.dram.bin \
run_meminfo_flash \


