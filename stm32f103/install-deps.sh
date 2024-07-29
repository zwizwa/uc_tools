#!/bin/bash
cd $(dirname $0)
. ./env.common.sh

LIBOPENCM3_HASH=3eff201a4bb3759a9c967a6f5e3fd0aad46dc5af
GCC_TAR=gcc-arm-none-eabi-4_8-2014q2-20140609-linux.tar.bz2
GCC_URL=https://launchpad.net/gcc-arm-embedded/4.8/4.8-2014-q2-update/+download/$GCC_TAR

if [ ! -d $LIBOPENCM3 ]; then
    echo Downloading libopencm3
    git clone https://github.com/libopencm3/libopencm3.git $LIBOPENCM3
    (cd $LIBOPENCM3 ; git checkout $LIBOPENCM3_HASH) || exit 1
fi

if [ ! -d $TOOL_DIR ]; then
    if [ ! -e $GCC_TAR ]; then
        echo Downloading compiler
        echo "TOOL_DIR=$TOOL_DIR"
        wget "$GCC_URL" || exit 1
    fi
    echo Unpacking compiler
    GCC_TAR_ABS=$(readlink -f $GCC_TAR)
    (cd ../../ ; tar xf $GCC_TAR_ABS) || exit 1
    rm -f $GCC_TAR
fi

exec make -C $LIBOPENCM3 PREFIX=$(readlink -f $TPF)- TARGETS="stm32/f1 stm32/f4" "$@"


# Derived from Makefile rule:
## $(LIBOPENCM3_F1_A): $(GCC) $(LIBOPENCM3)/README.md
##	make -C $(LIBOPENCM3) PREFIX=$(shell readlink -f $(TPF)) TARGETS=stm32/f1
