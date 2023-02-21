#!/bin/sh

# $ qemu-system-arm -cpu cortex-m3 -machine help 2>&1 |grep M3
# lm3s6965evb          Stellaris LM3S6965EVB (Cortex-M3)
# lm3s811evb           Stellaris LM3S811EVB (Cortex-M3)
# mps2-an385           ARM MPS2 with AN385 FPGA image for Cortex-M3
# mps2-an505           ARM MPS2 with AN505 FPGA image for Cortex-M33
# mps2-an511           ARM MPS2 with AN511 DesignStart FPGA image for Cortex-M3
# mps2-an521           ARM MPS2 with AN521 FPGA image for dual Cortex-M33
# mps3-an524           ARM MPS3 with AN524 FPGA image for dual Cortex-M33
# musca-a              ARM Musca-A board (dual Cortex-M33)
# musca-b1             ARM Musca-B1 board (dual Cortex-M33)
# netduino2            Netduino 2 Machine (Cortex-M3)
# stm32vldiscovery     ST STM32VLDISCOVERY (Cortex-M3)

# file:///nix/store/kw52sdy2c5nqsx54mann2pfj8hdmx9j3-qemu-6.2.0/share/doc/qemu/qemu/system/arm/stm32.html

# KERNEL=mem.bin
KERNEL=../gdb/qemu.core.f103.bin

## Doesn't work
# system() {
#     exec qemu-system-arm \
#          -s \
#          -kernel $KERNEL \
#          -machine stm32vldiscovery \
#          -nographic \
#          -chardev stdio,mux=off,id=char0  \
#          -mon chardev=char0,mode=readline \
#          -serial chardev:char0 \
#          -action panic=pause \
#          "$@"
# }

## Took a while to find the right incantation until I found the docs
## in /nix/store which mentioned the -kernel option.  This set of
## startup parameters works with the gdb/qemu.c inifinite loop and
## gdb/1234.sh gdb connect script.

system() {
    exec qemu-system-arm \
         -gdb tcp::1234 \
         -kernel $KERNEL \
         -machine stm32vldiscovery \
         -nographic \
         "$@"
}

system "$@"

