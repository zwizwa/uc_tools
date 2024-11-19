#!/bin/sh
if [ -z "$ESPPORT" ]; then
    USB=3-6.2:1.0
    DEV=$(find /sys/devices -name $USB)
    ESPPORT=/dev/$(ls $DEV/ttyUSB*/tty)
fi
echo ESPPORT=$ESPPORT
export ESPPORT

