#!/bin/bash
# $1 target
# $2 basename
# $3 temp
# echo "0:$0 args:$@" >>/tmp/redo.log

if [ ! -z "REDO_VERBOSE_ENTER" ]; then 
    echo "redo: Entering directory '$(readlink -f .)'" >&2
fi

redo-ifchange $2.c
. ./env.sh
echo "CFLAGS=$CFLAGS" >&2
$GCC $CFLAGS -MD -MF $3.deps.tmp -DFIRMWARE=\"$2\" -DBUILD=\"$VERSION\" -o $3 -c $2.c || exit 1

# Transform the Makefile style dependency list into just a list of
# prerequisites.
DEPS=$(sed -e "s/^$3://" -e 's/\\//g' <$3.deps.tmp)
rm -f $3.deps.tmp
redo-ifchange $DEPS
