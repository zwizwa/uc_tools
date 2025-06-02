#!/bin/sh
WEBROOT=$(readlink -f $(dirname "$0"))
cd "${WEBROOT}/../../lua"
exec ./webapp_dataflow.lua $WEBROOT

