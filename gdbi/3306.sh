#!/bin/sh
HERE=$(dirname $0)
dtach -A $HERE/3306.dtach $HERE/gdbi.sh tx 3306
