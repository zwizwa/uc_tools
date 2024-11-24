#!/bin/sh

case $(hostname) in
    zoe)
        ESPPORT=/dev/serial/by-id/usb-1a86_USB_Single_Serial_589B038622-if00
        ;;
    mimas)
	ESPPORT=/dev/serial/by-id/usb-1a86_USB_Single_Serial_589B038284-if00
        ;;
esac
if [ -z "$ESPPORT" ]; then
    ESPPORT=/dev/ttyACM0
fi
echo ESPPORT=$ESPPORT
export ESPPORT

