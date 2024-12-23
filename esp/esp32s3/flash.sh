#!/bin/sh
cd $(dirname "$0")
. ./espport.sh

## This is very slow over NFS.
#./ninja.sh flash

set -x

# Instead, run the command as ninja would run it and assume all files are up to date.
cd build

# Some visual time stamp feedback.
date
ls -l \
bootloader/bootloader.bin \
myProject.bin \
partition_table/partition-table.bin \
ota_data_initial.bin \

cat flash_args


../../run.sh esptool.py -p $ESPPORT --chip esp32s3 --before=default_reset --after=hard_reset write_flash `cat flash_args`
