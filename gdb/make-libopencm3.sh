#!/bin/bash
cd $(dirname $0)
. ./env.common.sh

HASH=3eff201a4bb3759a9c967a6f5e3fd0aad46dc5af
if [ ! -d $LIBOPENCM3 ]; do
    git clone https://github.com/libopencm3/libopencm3.git $LIBOPENCM3
    (cd $LIBOPENCM ; git checkout $HASH)
fi

exec make -C $LIBOPENCM3 PREFIX=$(readlink -f $TPF)- TARGETS="stm32/f1 stm32/f4" "$@"


# Derived from Makefile rule:
## $(LIBOPENCM3_F1_A): $(GCC) $(LIBOPENCM3)/README.md
##	make -C $(LIBOPENCM3) PREFIX=$(shell readlink -f $(TPF)) TARGETS=stm32/f1
