#!/bin/sh
cd $(dirname "$0")

# ESPPORT="socket://localhost:12345"
ESPPORT="rfc2217://localhost:12345"




cd build
../../run.sh esptool.py -p $ESPPORT --chip esp32s3 --before=default_reset --after=hard_reset write_flash `cat flash_args`
