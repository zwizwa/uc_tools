ARCH="${2##*.}"

# Base names of C files.  Note that the .o rule is smart enough to
# find the file in the correct directory.

C="
cdcacm_desc.c
gdbstub.c
memory.c
rsp_packet.c
vector.c
bootloader.c
hw_bootloader.c
sm_etf.c
infof.c
tools.c
info_null.c
cbuf.c
slipstub.c
mdio.c
"

O=$(echo $C | sed s/\\.c/\\.${ARCH}\\.o/g)

# User-defined library extensions
echo "UC_TOOLS_LIB_DIR=$UC_TOOLS_LIB_DIR" >&2

if [ ! -z "$UC_TOOLS_LIB_DIR" ]; then
    O_EXTRA=$(cd $UC_TOOLS_LIB_DIR ; ls *.c | sed s/\\.c/\\.${ARCH}\\.o/g)
    O="$O $O_EXTRA"
fi

echo "O=$O" >&2


cat <<EOF >&2
ARCH=$ARCH
O=$O
EOF

redo-ifchange $O

ar -r $3 $O 2>/dev/null


