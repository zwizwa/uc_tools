#!/bin/sh

IP=192.168.6.195


CMD="/i/tom/rdm-bridge/tools/tether_bl.dynamic.host.elf $IP"

ADDR=0x3FCD8000
$CMD load $ADDR /i/exo/uc_tools/esp32c3/plugin/test.bin
$CMD run_ram $ADDR

