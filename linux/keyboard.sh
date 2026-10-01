#!/bin/sh

# DEV="/dev/input/by-id/usb-Lenovo_Lenovo_Traditional_USB_Keyboard-event-kbd"
# DEV="/dev/input/by-id/usb-CHICONY_USB_NetVista_Full_Width_Keyboard-event-kbd"

# Pick the first one if it's not in the environment.
[ -z "$DEV" ] && DEV=$(ls /dev/input/by-id/*-kbd | head -n1)

exec $(dirname "$0")/keyboard.dynamic.host.elf "$DEV" "$@"
