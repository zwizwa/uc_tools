#!/bin/bash
# Stub script for do.erl
DIR=/i/exo/uc_tools/lua
echo "Entering directory: $DIR"
cd $DIR
exec ./$(basename $0 .sh).lua

