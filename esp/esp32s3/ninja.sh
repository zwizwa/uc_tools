#!/bin/sh
cd $(dirname "$0")/build
exec ../../run.sh ninja "$@"


