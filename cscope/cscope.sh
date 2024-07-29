#!/bin/bash

# What I want: a cscope file with just the relevant C files.  This is
# sometimes difficult for things like HAL libraries and linux drivers,
# because only a part of the code is used.  I've not found a better
# way to do this than to either list everything manually, or do a mix
# between globbing and manual as done here.

cd $(dirname $0)
rm -f cscope.in.out cscope.out cscope.po.out cscope.files

LIBOPENCM3=$(readlink -f .)/../../libopencm3

cat <<EOF >cscope.files
$(ls ../stm32f103/*.[ch])
$(find $LIBOPENCM3 -name '*.c')
$(find $LIBOPENCM3 -name '*.h')
EOF

cscope -kbq
