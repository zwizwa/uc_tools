#!/bin/sh
cd $(dirname "$0")
ESPPORT=/dev/ttyUSB0 idf.py monitor
