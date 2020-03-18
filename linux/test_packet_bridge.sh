cd $(dirname $0)
[-z "$ELF" ] && ELF=./packet_bridge.dynamic.linux.elf
$ELF

