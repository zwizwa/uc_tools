# Rebuild libopencm3

redo-ifchange env.common.sh
. ./env.common.sh

# Arbitrary check to make sure dir is there.
redo-ifchange $LIBOPENCM3/README.md

./make-libopencm3.sh
