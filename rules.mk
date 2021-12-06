# Generalized from the exo-generated file gen.mk, using the same API
# on top of */build.sh
#
# This Makefile is structured to build multiple target archtectures
# from the same source.  To make that problem manageable, the build is
# split into two parts:
#
# - This is rules.mk file that expresses dependencies and provides a
#   standard Makefile interface.
#
# - The build.sh script that contains details for building one binary
#   target at a time. (o,a,elf,so).
#
# More specifically:
#
# - this file contains rules that set parameters used by the build.sh
#   scripts
#
# - a single makefile is used to build everything
#
# - to facilitate rule encoding, architecture information is encoded
#   in file names (stm32f103,host)
#
# - for elf files, in addition the link type (linker script) is
#   encoded in the filename (dynamic,x8,core)
#
# - .o files are collected in an .a file per platform, and each .elf
#   is linked against that .a file






# A note about firmware versions.
#
# The version.sh script is run at .f103.elf link time to produce a
# version string on stdout, which is then converted to a .c file,
# compiled to .o and linked into the executable.  See VERSION_LINK_GEN
# below, which passed the script name to uc_tools/gdb/build.sh



# A quick note on formats:

# - The simply named .hex files such as main_uvc.hex are for initial
#   flashing of boards, they contain the trampoline + firmware
#   duplicated over the two partitions.
#
# - The .fw.bin files are for firmware upgrade over RDM
#
# - The .elf files are for gdb debugging.  .elf and .fw.elf differ,
#   with the latter having a populated firmware control block.


STM_ELF := \
	gdb/bl_c8t6_a12b5_boot1.core.f103.elf \
	gdb/pdap.x8.f103.elf \

STM_HEX_BIN := \
	$(BIN2FW)

HOST_ELF := \
	linux/gdbstub_connect.dynamic.host.elf \

ALL_ELF := $(STM_HEX_BIN) $(STM_ELF) $(KLSTR_ELF) $(HOST_ELF)

UC_TOOLS := .

LIB_F103_A_OBJECTS := \
	gdb/bootloader.f103.o \
	gdb/cdcacm_desc.f103.o \
	gdb/gdbstub.f103.o \
	gdb/hw_bootloader.f103.o \
	gdb/memory.f103.o \
	gdb/pluginlib.f103.o \
	gdb/rsp_packet.f103.o \
	gdb/sm_etf.f103.o \
	gdb/vector.f103.o \
	gdb/instance.f103.o \
	gdb/stack.f103.o \
	gdb/semihosting.f103.o \
	memoize.f103.o \
	\
	csp.f103.o \
	cbuf.f103.o \
	info_null.f103.o \
	infof.f103.o \
	mdio.f103.o \
	pbuf.f103.o \
	sliplib.f103.o \
	slipstub.f103.o \
	tag_u32.f103.o \
	tools.f103.o \
	cycle_counter.f103.o \


LIB_HOST_A_OBJECTS := \
	cbuf.host.o \
	csp.host.o \
	info_null.host.o \
	infof.host.o \
	mdio.host.o \
	pbuf.host.o \
	sliplib.host.o \
	tools.host.o \
	cycle_counter.host.o \
	linux/packet_bridge.host.o \

# Generated files

LUA := $(shell find lua -name '*.lua')

# Linker file
gdb/%.ld: gdb/%.ld.sh
	@echo $@ ; if [ -f env.sh ] ; then . ./env.sh ; fi ; \
	export BUILD=gdb/build.sh ; \
	export LD=$@ ; \
	export LD_GEN=$< ; \
	export TYPE=ld ; \
	export UC_TOOLS=$(UC_TOOLS) ; \
	$$BUILD 2>&1


# Object files that go into .a or are linked directly into the .elf
%.f103.o: %.c $(GEN)
	@echo $@ ; if [ -f env.sh ] ; then . ./env.sh ; fi ; \
	export ARCH=f103 ; \
	export BUILD=gdb/build.sh ; \
	export C=$< ; \
	export CFLAGS_EXTRA="-Igdb/ -Ilinux/" ; \
	export D=$(patsubst %.o,%.d,$@) ; \
	export FIRMWARE=memory ; \
	export O=$@ ; \
	export TYPE=o ; \
	export UC_TOOLS=$(UC_TOOLS) ; \
	$$BUILD 2>&1

# Main object library.  All elf files link against this.
gdb/lib.f103.a: $(LIB_F103_A_OBJECTS) rules.mk
	@echo $@ ; if [ -f env.sh ] ; then . ./env.sh ; fi ; \
	export A=$@ ; \
	export BUILD=gdb/build.sh ; \
	export OBJECTS="$(LIB_F103_A_OBJECTS)" ; \
	export TYPE=a ; \
	export UC_TOOLS=$(UC_TOOLS) ; \
	$$BUILD 2>&1

# Firmware application binaries for the GDBSTUB bootloader.  Note that
# all weak symbols will need to go into the main .c file.  This
# includes libopencm3 interrupt vectors.  The registers_* files are
# just memory-mapped device layout for debugging.
%.x8.f103.elf: \
	%.f103.o \
	gdb/lib.f103.a \
	gdb/x8.f103.ld \
	../uc_tools/gdb/registers_stm32f103.f103.o \

	@echo $@ ; if [ -f env.sh ] ; then . ./env.sh ; fi ; \
	export A=gdb/lib.f103.a ; \
	export ARCH=f103 ; \
	export BUILD=gdb/build.sh ; \
	export ELF=$@ ; \
	export LD=gdb/x8.f103.ld ; \
	export MAP=$(patsubst %.elf,%.map,$@) ; \
	export O=$< ; \
	export TYPE=elf ; \
	export UC_TOOLS=../uc_tools/ ; \
	export VERSION_LINK_GEN=./version.sh ; \
	$$BUILD 2>&1

# Core binaries.  This is e.g. for bootloader.
%.core.f103.elf: \
	%.f103.o \
	gdb/lib.f103.a \
	gdb/core.f103.ld \
	../uc_tools/gdb/registers_stm32f103.f103.o \

	@echo $@ ; if [ -f env.sh ] ; then . ./env.sh ; fi ; \
	export A=gdb/lib.f103.a ; \
	export ARCH=f103 ; \
	export BUILD=gdb/build.sh ; \
	export ELF=$@ ; \
	export LD=gdb/core.f103.ld ; \
	export MAP=$(patsubst %.elf,%.map,$@) ; \
	export O=$< ; \
	export TYPE=elf ; \
	export UC_TOOLS=$(UC_TOOLS) ; \
	$$BUILD 2>&1



# Raw binary from elf
%.bin: \
	%.elf \

	@echo $@ ; if [ -f env.sh ] ; then . ./env.sh ; fi ; \
	export ARCH=f103 ; \
	export BUILD=gdb/build.sh ; \
	export ELF=$< ; \
	export BIN=$@ ; \
	export TYPE=bin ; \
	export UC_TOOLS=$(UC_TOOLS) ; \
	$$BUILD 2>&1

# Intel HEX from elf
%.hex: \
	%.elf \

	@echo $@ ; if [ -f env.sh ] ; then . ./env.sh ; fi ; \
	export ARCH=f103 ; \
	export BUILD=gdb/build.sh ; \
	export ELF=$< ; \
	export HEX=$@ ; \
	export TYPE=hex ; \
	export UC_TOOLS=$(UC_TOOLS) ; \
	$$BUILD 2>&1




# HOST

%.host.o: %.c $(GEN)
	@echo $@ ; if [ -f env.sh ] ; then . ./env.sh ; fi ; \
	export ARCH=host ; \
	export BUILD=linux/build.sh ; \
	export C=$< ; \
	export CFLAGS_EXTRA="-O3 -Igdb/ -Ilinux/" ; \
	export D=$(patsubst %.o,%d,$@) ; \
	export FIRMWARE=$$(basename $< .c) ; \
	export O=$@ ; \
	export TYPE=o ; \
	export UC_TOOLS=$(UC_TOOLS) ; \
	$$BUILD 2>&1


linux/lib.host.a: $(LIB_HOST_A_OBJECTS)
	@echo $@ ; if [ -f env.sh ] ; then . ./env.sh ; fi ; \
	export A=linux/lib.host.a ; \
	export BUILD=linux/build.sh ; \
	export OBJECTS="$(LIB_HOST_A_OBJECTS)" ; \
	export TYPE=a ; \
	export UC_TOOLS=$(UC_TOOLS) ; \
	$$BUILD 2>&1

%.dynamic.host.elf: \
	%.host.o \
	linux/lib.host.a \

	@echo $@ ; if [ -f env.sh ] ; then . ./env.sh ; fi ; \
	export A=linux/lib.host.a ; \
	export ARCH=host ; \
	export BUILD=linux/build.sh ; \
	export ELF=$@ ; \
	export LD=linux/dynamic.host.ld ; \
	export MAP=$(patsubst %.elf,%.map,$@) ; \
	export O=$< ; \
	export LDLIBS="" ; \
	export TYPE=elf ; \
	export UC_TOOLS=$(UC_TOOLS) ; \
	$$BUILD 2>&1

%.dynamic.host.so: \
	%.host.o \
	linux/lib.host.a \

	@echo $@ ; if [ -f env.sh ] ; then . ./env.sh ; fi ; \
	export A=linux/lib.host.a ; \
	export ARCH=host ; \
	export BUILD=linux/build.sh ; \
	export SO=$@ ; \
	export LD=linux/dynamic.host.ld ; \
	export LDLIBS="-ldw -lelf" ; \
	export MAP=$(patsubst %.so,%.map,$@) ; \
	export O=$< ; \
	export TYPE=so ; \
	export UC_TOOLS=$(UC_TOOLS) ; \
	$$BUILD 2>&1



# E.g. for clean target

ALL_PRODUCTS := \
	$(LIB_F103_A_OBJECTS) \
	$(LIB_HOST_A_OBJECTS) \
	$(ALL_ELF) \
	gdb/lib.f103.a \
	linux/lib.host.a \
