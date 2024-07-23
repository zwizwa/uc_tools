#!/bin/sh

CMD="/i/tom/rdm-bridge/tools/tether_bl.dynamic.host.elf 192.168.0.122"

$CMD load 0x40098000 /i/exo/uc_tools/esp32/plugin/test.iram.bin
$CMD load 0x3FFF8000 /i/exo/uc_tools/esp32/plugin/test.dram.bin
$CMD run_ram 0x40098000

