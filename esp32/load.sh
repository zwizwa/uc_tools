#!/bin/sh

# IP=192.168.0.122
IP=192.168.6.122


CMD="/i/tom/rdm-bridge/tools/tether_bl.dynamic.host.elf $IP"

$CMD load 0x40098000 /i/exo/uc_tools/esp32/plugin/test.iram.bin
$CMD load 0x3FFF8000 /i/exo/uc_tools/esp32/plugin/test.dram.bin
$CMD run_ram 0x40098000

