#!/bin/sh

case $(hostname) in
    zoe)
        USB=2-1.5.2:1.0
        ;;
    mimas)
        USB=3-6.1.1:1.0
        ;;
esac


if [ -z "$ESPPORT" ]; then
    DEV=$(find /sys/devices -name $USB)
    ESPPORT=/dev/$(ls $DEV/tty)
fi
echo ESPPORT=$ESPPORT
export ESPPORT

