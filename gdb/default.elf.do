# $1 target
# $2 basename
# $3 temp

# We're building files like this:
# redo bl_c8t6.core.f103.elf
#      BN      LDT  ARCH

# BN:   basename
# LDT:  linker type (.ld file)
# ARCH: compilation architecture (.o files)



export ARCH="${2##*.}"
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

[ -z "$UC_TOOLS" ] && export UC_TOOLS=..

# show_vars

redo-ifchange $ENV
. $ENV


# .o files that need to be linked in explicitly. contianing the
# application elf's main and optionally some system .o files
# configured in $ENV file.
export O="$BN.$ARCH.o"

# library of .o files for compilation architecture for any remaining unresolved symbols
export A=lib.$ARCH.a

redo-ifchange $O_SYSTEM
redo-ifchange $O $A


export LD=$LDT.$ARCH.ld

if [ -f "$LD" ]; then
    redo-ifchange $LD 
    export E=$3
    export MAP=$BN.$ARCH.map
    redo-ifchange build.elf.sh
    ./build.elf.sh
    
else
    # no .ld file, use a custom link step instead.

    # Try basename of the .c file
    LINK_SH="./$BN.link.sh"

    # If that doesn't exist, use the default plugin link script, which
    # used the LDT to identify the parent elf.
    [ ! -f "$LINK_SH" ] && LINK_SH=./plugin.link.sh

    # Source the shell script fragment to perform the linking.  This
    # allows it to be parameterized by the variables we already have
    # defined here.
    echo "plugin link: $LINK_SH" >&2
    redo-ifchange $LINK_SH
    . $LINK_SH
fi


