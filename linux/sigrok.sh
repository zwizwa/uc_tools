#!/bin/sh
# - sigrok-cli stops on stdin EOF, e.g. /dev/null in a service
sleep infinity | sigrok-cli -d fx2lafw -O binary --continuous --config samplerate=2m
