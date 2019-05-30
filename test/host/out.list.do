# Tests that can run on host only.
for c in $(ls *.c); do
    echo $(basename $c .c).log.txt
done

