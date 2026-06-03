#!/bin/sh
exec sigrok-cli -d fx2lafw -O binary --continuous --config samplerate=2m
