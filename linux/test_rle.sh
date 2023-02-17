#!/bin/sh
echo 'AAAABBCDDDD' | ./rle_enc.dynamic.host.elf | ./rle_dec.dynamic.host.elf | hexdump -C


ENC=./rle_enc.dynamic.host.elf
DEC=./rle_dec.dynamic.host.elf

echo 'AAAABBCDDDD' | $ENC | $DEC | hexdump -C

S="A"
S="$S$S";S="$S$S";S="$S$S";S="$S$S";S="$S$S";S="$S$S";S="$S$S";S="$S$S";

S1="BBB$S"
echo $S1
echo $S1 | $ENC | hexdump -C
echo $S1 | $ENC | $DEC
echo $S1 | $ENC | $DEC | $ENC | hexdump -C


S="$S$S";S="$S$S";S="$S$S";S="$S$S";S="$S$S";
S="$S$S";S="$S$S";S="$S$S";S="$S$S";S="$S$S";

echo $S | $ENC | hexdump -C
echo $S | $ENC | $DEC | $ENC | hexdump -C

