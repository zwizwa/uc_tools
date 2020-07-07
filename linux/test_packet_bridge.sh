# FIXME: Is this still used?  See rule for packet_bridge.test.sh
cd $(dirname $0)
[-z "$ELF" ] && ELF=./packet_bridge.dynamic.linux.elf
$ELF

