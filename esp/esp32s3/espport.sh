#!/bin/sh
if [ -z "$ESPPORT" ]; then
    USB=3-6.1.1:1.0
    DEV=$(find /sys/devices -name $USB)
    ESPPORT=/dev/$(ls $DEV/tty)
fi
echo ESPPORT=$ESPPORT
export ESPPORT

