#!/bin/bash
. $(dirname $0)/buildlib.sh
need_vars A O
ar -r $A $O 2>/dev/null
