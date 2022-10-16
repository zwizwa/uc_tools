#!/bin/sh

# Use make to perform parallel runs with different seed values.  To
# make this efficient, make sure that the startup time of the Lua
# interpreter is much smaller than the run time of the test.  A test
# run time of about a second is probably ok.

cd $(dirname "$0")
cat <<EOF | make -f /dev/stdin -j $(nproc) all
all: $(for i in $(seq 1 100) ; do echo -ne "SEED_$i " ; done)
SEED_%:
	SEED=\$* ./qcrun.lua 2>/dev/null
EOF

