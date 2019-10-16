# default.elf.do supports ad-hoc linking stages
# whenver $LDT==custom, $BN.link.sh is sourced into the build script

# The main reason to perform custom linking is to allow for "plugins"
# that can be loaded along side a main application when the app is
# running, and that can call into the application code as well.
# This is useful for incremental development, avoiding restarts.

PARENT_ELF=doodle.x8.f103.elf
redo-ifchange $PARENT_ELF

redo-ifchange mem_top.sh
LD=$3.tmp.ld
cat <<EOF >$LD
$(./mem_top.sh $PARENT_ELF)
INCLUDE stm32f1.ld
EOF

$GCC $LDFLAGS -T$LD -Wl,-Map=$BN.$ARCH.map -Wl,--just-symbols=$PARENT_ELF -o $3 $O $A $LDLIBS || exit $?

rm $LD

arm-none-eabi-objdump -d $3 >&2
# arm-none-eabi-objdump -d $PARENT_ELF >&2

