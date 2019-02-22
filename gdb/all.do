#!/bin/bash
# C=$(ls *.c)
# O=$(echo $C | sed s/\\.c/\\.o/g)
# redo-ifchange $O
# redo-ifchange lib.a

ELF="
bl_hyministm32v.core.elf
bl_hyministm32v_autostart.core.elf
bl_c8t6.core.elf
relay_board.x8.elf
gpio_etf.x8.elf
wavegen.x8.elf
etf_test.x8.elf
echo_test.x8.elf
"


redo-ifchange $ELF || exit 1

# for elf in $ELF; do file $elf >&2 done

