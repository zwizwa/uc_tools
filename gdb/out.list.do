redo-ifchange env.sh
. ./env.sh
# The convention is one file per line
for elf in $ELF; do
    echo $elf >>$3
done
