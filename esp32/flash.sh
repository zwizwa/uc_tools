#!/bin/sh
cd $(dirname "$0")/build
ESPPORT=/dev/ttyUSB0 ninja flash
