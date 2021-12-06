# Minimal script to support installation and access via 'uc_tools' script
# See ./uc_tools for more information.


INSTALL_SO="linux/elfutils_lua51.dynamic.host.so"
INSTALL_ELF="linux/packet_bridge_main.dynamic.host.elf"

all: $(INSTALL_SO) $(INSTALL_ELF)

PREFIX ?= /usr/local/uc_tools
install: all
	mkdir -p $(PREFIX)/lua
	mkdir -p $(PREFIX)/bin
	cp -a linux/elfutils_lua51.dynamic.host.so $(PREFIX)/lua/elfutils_lua51.so
	cp -a lua/lib/elfutils.lua                 $(PREFIX)/lua/
	cp -a bin/uc_tools                         $(PREFIX)/bin/

include rules.mk


