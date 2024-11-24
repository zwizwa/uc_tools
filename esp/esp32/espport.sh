#!/bin/sh

ESPPORT=/dev/serial/by-id/usb-Silicon_Labs_CP2102_USB_to_UART_Bridge_Controller_0001-if00-port0
# ESPPORT=/dev/serial/by-path/pci-0000:00:14.0-usbv2-0:6.2:1.0-port0
[ -z "$ESPPORT" ] && ESPPORT=/dev/ttyUSB0
echo ESPPORT=$ESPPORT
export ESPPORT

