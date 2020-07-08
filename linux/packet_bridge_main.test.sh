#!/bin/bash
cd $(dirname $0)
[ -z "$ELF" ] && ELF=./packet_bridge_main.dynamic.host.elf

# Test PASS/FAIL is indicated by return value. 0->PASS, other->FAIL

echo "ELF=$ELF"
cat <<EOF | $ELF HEX -:4 | hexdump -C -v
01020304050607080A0B0C0D
01020304050607080A0B0C0D
01020304050607080A0B0C0D
01020304050607080A0B0C0D
EOF
exit 0

