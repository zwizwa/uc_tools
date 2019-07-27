#!/bin/bash
cd $(dirname $0)
. ./env.common.sh
exec make -C $LIBOPENCM3 PREFIX=$(readlink -f $TPF) TARGETS=stm32/f1 "$@"


# Derived from Makefile rule:
## $(LIBOPENCM3_F1_A): $(GCC) $(LIBOPENCM3)/README.md
##	make -C $(LIBOPENCM3) PREFIX=$(shell readlink -f $(TPF)) TARGETS=stm32/f1
