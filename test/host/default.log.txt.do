# Create .log.txt file for reach C file

# So emacs compile mode knows where we are.
if [ ! -z "REDO_VERBOSE_ENTER" ]; then 
    echo "redo: Entering directory '$(readlink -f .)'" >&2
fi

redo-ifchange $2.c
gcc -Werror -Wall -o $3.elf $2.c -I../../
$(readlink -f $3.elf) >$3
rm -f $3.elf
