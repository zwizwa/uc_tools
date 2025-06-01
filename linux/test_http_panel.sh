#!/bin/sh
cd $(dirname "$0")
echo "try: window.mod.ws.test() in web console"
./http_panel.dynamic.host.elf ../webapp/panel/
