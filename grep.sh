#!/bin/bash
# Is it better to put this config in emacs?  It's useful to save it as
# a script that is a "method of this directory", i.e. this script
# knows what is in the directory, and knows how to search it (only .c
# and .h files, ignore all the rest).
[ -z "$1" ] && echo "usage: $0 <pattern>" && exit 1
grep -nrI *.[ch] -e "$1"
