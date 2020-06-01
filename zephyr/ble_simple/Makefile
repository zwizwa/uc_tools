.PHONY: all clean menuconfig flash run
all: build/zephyr/zephyr.elf

# BUILD=ninja
# CONFIGURED=build/build.ninja
# SELECT_TOOL=-GNinja

BUILD=make 
CONFIGURED=build/Makefile
SELECT_TOOL=


build/zephyr/zephyr.elf: $(CONFIGURED) src/main.c
	. ./env.sh ; $(BUILD) -C build
	ls -l $@


# Use 'D' optin in menuconfig to save ./build/zephyr/kconfig/defconfig
menuconfig: $(CONFIGURED)
	. ./env.sh ; $(BUILD) -C build menuconfig

# BOARD=stm32_min_dev_blue
BOARD=nrf52840_mdk

build/Makefile: env.sh
	. ./env.sh ; mkdir -p build && cd build && cmake $(SELECT_TOOL) -DBOARD=$(BOARD) ..


clean:
	rm -rf build *~ cscope.files cscope.in.out cscope.out cscope.po.out

# User our own flash
flash: build/zephyr/zephyr.elf flash.gdb
	arm-eabi-gdb --quiet -x flash.gdb

run:
	arm-eabi-gdb --quiet -x run.gdb

