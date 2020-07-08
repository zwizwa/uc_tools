#!/bin/bash
cd $(dirname $0)
[ -z "$ELF" ] && ELF=$(readlink -f ./packet_bridge_main.dynamic.host.elf)

# Test PASS/FAIL is indicated by return value. 0->PASS, other->FAIL

test_hex() {

echo "ELF=$ELF"
cat <<EOF | $ELF HEX -:4 | hexdump -C -v
01020304050607080A0B0C0D
01020304050607080A0B0C0D
01020304050607080A0B0C0D
01020304050607080A0B0C0D
EOF

}

test_usb() {

set -x

# FIXME: This has been postponed in favor of a dedicated program for axo.
# FIXME: This has to run on a physical host.
ABS_ELF="/i/$HOSTNAME/$ELF"
echo ABS_ELF=$ABS_ELF
(ssh 10.1.3.12 $ABS_ELF - USB) 2>&1

}

test_hex


exit 0

