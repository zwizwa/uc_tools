#!/bin/sh
cd $(dirname "$0")
mkdir -p components
cd components
git clone https://github.com/someweisguy/esp_dmx.git
