#!/bin/sh

# ESPPORT=/dev/serial/by-id/usb-1a86_USB_Serial-if00-port0
ESPPORT=/dev/serial/by-path/pci-0000:00:14.0-usb-0:6.1.2:1.0-port0
[ -z "$ESPPORT" ] && ESPPORT=/dev/ttyUSB0
echo ESPPORT=$ESPPORT
export ESPPORT

