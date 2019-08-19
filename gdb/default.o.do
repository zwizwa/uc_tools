#!/bin/bash
# $1 target
# $2 basename
# $3 temp
# echo "0:$0 args:$@" >>/tmp/redo.log

# So emacs compile mode knows where we are.
if [ ! -z "REDO_VERBOSE_ENTER" ]; then 
    echo "redo: Entering directory '$(readlink -f .)'" >&2
fi


ARCH="${2##*.}"
BN=$(basename $2 .$ARCH)

# Find the corresponding C file.
# echo "UC_TOOLS_APP_DIR=$UC_TOOLS_APP_DIR" >&2
# echo "UC_TOOLS_LIB_DIR=$UC_TOOLS_LIB_DIR" >&2

C=$BN.c
[ ! -f $C ] && C=../$BN.c
[ ! -f $C ] && C=$UC_TOOLS_LIB_DIR/$BN.c
[ ! -f $C ] && C=$UC_TOOLS_APP_DIR/$BN.c
[ ! -f $C ] && exit 1

echo "C=$C" >&2

redo-ifchange $C
ENV=./env.$ARCH.sh
redo-ifchange $ENV
. $ENV

if [ ! -z "$UC_TOOLS_LIB_DIR" ]; then
    CFLAGS="-I$UC_TOOLS_LIB_DIR $CFLAGS"
    echo "CFLAGS=$CFLAGS" >&2
fi




# cat <<EOF >&2
# 1=$1
# 2=$2
# 3=$3
# ARCH=$ARCH
# BN=$BN
# C=$C
# ENV=$ENV
# CFLAGS=$CFLAGS
# EOF

$GCC $CFLAGS -MD -MF $3.deps.tmp -DFIRMWARE=\"$BN\" -DBUILD=\"$VERSION\" -o $3 -c $C || exit 1

# Transform the Makefile style dependency list into just a list of
# prerequisites.
DEPS=$(sed -e "s/^$3://" -e 's/\\//g' <$3.deps.tmp)
rm -f $3.deps.tmp
redo-ifchange $DEPS
