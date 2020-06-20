#!/bin/sh
# Assume only the correct architecture is installed.
# FIXME: Needs review.
ELF=$(ls $(dirname $0)/klog.dynamic.*.elf)
killall klogd
exec $ELF

