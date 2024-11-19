#!/bin/sh

# There is no uniformity here.  Some have serial number, some have
# none, some have one like '0001'.  So identify them by port number and just change whenever they get replugged

# DEV=/sys/devices/pci0000:00/0000:00:14.0/usb3/3-6/3-6.1/3-6.1.1/3-6.1.1:1.0
USB=3-6.1.1:1.0
DEV=$(find /sys/devices -name $USB)
TTY=/dev/$(ls $DEV/tty)
echo $TTY
