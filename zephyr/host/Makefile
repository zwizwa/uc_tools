.PHONY: all
all: test.elf

test.elf: Makefile test.c ../ble_appdata/src/main.c ../ble_appdata/src/spec.h  ../ble_appdata/src/spec_impl.h ../ble_appdata/src/appdata.h  ../ble_appdata/src/appdata.c
	gcc -Wall -Werror -g -I ../ble_appdata/src/ -o $@ test.c
