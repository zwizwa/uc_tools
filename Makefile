# Minimal script to support installation and access via 'uc_tools' script
# See ./uc_tools for more information.


ALL_LINUX=\
linux/elfutils_lua51.dynamic.host.so \
linux/packet_bridge_main.dynamic.host.elf \
linux/gdbstub_connect.dynamic.host.elf \

ALL_GDB=\
gdb/bl_c8t6_a12b5_boot1.core.f103.elf \

ALL ?= $(ALL_LINUX) $(ALL_GDB)

all: $(ALL)

PREFIX ?= /usr/local/uc_tools
install: all
	mkdir -p $(PREFIX)/gdb
	mkdir -p $(PREFIX)/linux
	mkdir -p $(PREFIX)/lua
	mkdir -p $(PREFIX)/bin
	cp -a $(ALL_GDB)                           $(PREFIX)/gdb/
	cp -a $(ALL_LINUX)                         $(PREFIX)/linux/
	cp -a lua/lib/elfutils.lua                 $(PREFIX)/lua/
	cp -a bin/uc_tools                         $(PREFIX)/bin/
	cd $(PREFIX)/lua ; ln -sf ../linux/elfutils_lua51.dynamic.host.so elfutils_lua51.so

include rules.mk


