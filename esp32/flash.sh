#!/bin/sh
cd $(dirname "$0")
ESPPORT=/dev/ttyUSB0 ./ninja.sh flash
