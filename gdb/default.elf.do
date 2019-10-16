# $1 target
# $2 basename
# $3 temp

# We're building files like this:
# redo bl_c8t6.core.f103.elf
#      BN      LDT  ARCH

# BN:   basename
# LDT:  linker type (.ld file)
# ARCH: compilation architecture (.o files)



ARCH="${2##*.}"
BN_LDT=$(basename $2 .$ARCH)
LDT="${BN_LDT##*.}"
BN=$(basename $BN_LDT .$LDT)
ENV=./env.$ARCH.sh

show_vars() {
cat <<EOF >&2
ARCH=$ARCH
LDT=$LDT
BN=$BN
ENV=$ENV
EOF
}

show_vars

redo-ifchange $ENV
. $ENV

# .o files that need to be linked in explicitly. contianing the
# application elf's main and optionally some system .o files
# configured in $ENV file.
O="$BN.$ARCH.o $O_SYSTEM"

# library of .o files for compilation architecture for any remaining unresolved symbols
A=lib.$ARCH.a

redo-ifchange $O $A



case $LDT in
     custom)
            # For custom linking, the filename maps to a shell script
            # fragment that performs the linking, parameterized by the
            # variables we already have above.
            LINK_SH="./$BN.link.sh"
            echo "custom link: $LINK_SH" >&2
            redo-ifchange $LINK_SH
            . $LINK_SH
            ;;
     *)
            # linker configuration file
            LD=$LDT.$ARCH.ld
            redo-ifchange $LD 
            $GCC $LDFLAGS -T$LD -Wl,-Map=$BN.$ARCH.map -o $3 $O $A $LDLIBS
            ;;
esac

