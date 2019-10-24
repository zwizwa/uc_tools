# See flash_plugin.link.sh for more info.

# This is a variant that puts all code in RAM.  Useful for frequent
# reloading that would otherwise stress Flash too much.

[ -z "$PARENT_ELF" ] && echo "need PARENT_ELF" >&2 && exit 1
echo "PARENT_ELF=$PARENT_ELF" >&2
redo-ifchange $PARENT_ELF

redo-ifchange mem_top.sh
LD=$3.tmp.ld
cat <<EOF >$LD
$(./mem_top.sh $PARENT_ELF)
INCLUDE ${ARCH}.ram.ld
EOF

$GCC $LDFLAGS -T$LD -Wl,-Map=$BN.$ARCH.map -o $3 $O -Wl,--just-symbols=$PARENT_ELF  $A $LDLIBS || exit $?

rm $LD



# arm-none-eabi-objdump -d $3 >&2
# arm-none-eabi-objdump -d $PARENT_ELF >&2

