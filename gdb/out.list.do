for arch in f103; do
    (redo-ifchange env.$arch.sh
     . ./env.$arch.sh
     # The convention is one file per line
     for elf in $ELF; do
         echo $elf >>$3
     done)
done
