# Some shared code to create a plugin linked to a PARENT_ELF

# See plugin_forth.link.sh as an example.

[ -z "$PARENT_ELF" ] && echo "need PARENT_ELF" && exit 1
echo "PARENT_ELF=$PARENT_ELF" >&2
redo-ifchange $PARENT_ELF

# This script will be sourced in the context of default.elf.do,
# defining the variables used below.

# General context of this script is default.elf.do, which supports
# ad-hoc linking stages: Whenever $LDT==custom, the script $BN.link.sh
# is sourced into the .do script.

# The main reason to perform custom linking is to allow for "plugins"
# that can be loaded along side a main application when the app is
# running, and that can call into the application code as well.  This
# is useful for incremental development, avoiding restarts.

redo-ifchange mem_top.sh
LD=$3.tmp.ld
cat <<EOF >$LD
$(./mem_top.sh $PARENT_ELF)
INCLUDE ${ARCH}.ld
EOF

# FIXME: It is actually possible here to put a checksum of the host
# elf's Flash contents inside the linker file, so it ends up in the
# binary.

$GCC $LDFLAGS -T$LD -Wl,-Map=$BN.$ARCH.map -o $3 $O -Wl,--just-symbols=$PARENT_ELF  $A $LDLIBS || exit $?

rm $LD



# arm-none-eabi-objdump -d $3 >&2
# arm-none-eabi-objdump -d $PARENT_ELF >&2

