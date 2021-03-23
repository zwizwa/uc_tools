#!/bin/bash
upload() {
    curl -T $1 http://192.168.0.123:3456/$1
}

upload_many() {
    for f in "$@"; do upload $f; done
}

if [ -z "$1" ]; then
    upload_many *.js *.html *.ico
else
    upload_many "$@"
fi
