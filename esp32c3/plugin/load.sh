#!/bin/sh
set -xe

# Always relink.
HERE=$(dirname "$0")
cd $HERE
make link_clean
make

# Load
# Note that FLASH=IRAM, RAM=DRAM
. ./meminfo.sh
./cmd.sh load $RAM_ADDR   ./test.dram.bin
./cmd.sh load $FLASH_ADDR ./test.iram.bin
./cmd.sh run_ram $FLASH_ADDR

