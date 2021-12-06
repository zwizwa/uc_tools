# FIXME: Minimal top-level Makefile to support exo/nix/uc_tools.nix

all: \
	linux/elfutils_lua51.dynamic.host.so

PREFIX ?= /usr/local
install:
	mkdir -p $(PREFIX)/uc_tools/lua
	cp -a linux/elfutils_lua51.dynamic.host.so $(PREFIX)/uc_tools/lua/elfutils_lua51.so
	cp -a lua/lib/elfutils.lua                 $(PREFIX)/uc_tools/lua/

include rules.mk


