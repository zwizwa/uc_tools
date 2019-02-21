C_LIB := \
	cdcacm_desc.c \
	gdbstub.c \
	memory.c \
	rsp_packet.c \
	vector.c \
	bootloader.c \
	hw_bootloader.c \
	sm_etf.c \
	../infof.c \
	../tools.c \
	info_null.c \

# FIXME: Applications will not have info support.  Replace info_null.c
# with a circular buffer.


C_OTHER := \
	bl_hyministm32v.c \
	bl_hyministm32v_autostart.c \
	bl_c8t6.c \

ELF := \
	bl_hyministm32v.core.elf \
	bl_hyministm32v_autostart.core.elf \
	bl_c8t6.core.elf \
	relay_board.x8.elf \
	gpio_etf.x8.elf \
	wavegen.x8.elf \
	etf_test.x8.elf \
	etf_console.x8.elf \
	echo_test.x8.elf

O_LIB := $(C_LIB:.c=.o)

all: $(ELF)

clean:
	rm -rf *~ *.o *.d *.dasm *.a *.elf *.push


# These are just some defaults.  Assume they are just next door.
TPF ?= ../../gcc-arm-none-eabi-4_8-2014q2/bin/arm-none-eabi
LIBOPENCM3 ?= ../../libopencm3
UC_TOOLS := ..
VERSION := $(shell git rev-parse HEAD)

# FIXME: move this into libprim instead
LIBPRIM ?= ../../libprim
LIBPRIM_LIBS ?= $(LIBPRIM)/build/gdbstub/lib.a

GCC      := $(TPF)-gcc
OBJDUMP  := $(TPF)-objdump
OBJCOPY  := $(TPF)-objcopy
READELF  := $(TPF)-readelf
HOST_GCC := gcc

# Don't.  This is too coarse-grained.
# MAKEFILE := Makefile
MAKEFILE :=

# Machine flags needed both for compiling and linking.
MFLAGS := \
	-mthumb \
	-mcpu=cortex-m3 \
	-msoft-float \
	-mfix-cortex-m3-ldrd

CPPFLAGS := \
	-I.. -I$(UC_TOOLS)/include \
	-I$(LIBOPENCM3)/include \
	-I$(LIBPRIM) \
	-DBUILD="\"$(VERSION)\""

CFLAGS := \
	-std=c99 \
	-fno-common \
	-ffunction-sections \
	-fdata-sections \
	-MD \
	-DSTM32F1 \
	-Wall \
	-Werror \
	-Wno-format \
	-Wno-attributes \
	-Wno-multichar \
	-g \
	-Os \
	$(CPPFLAGS) \
	$(MFLAGS)

LDFLAGS := \
	-g \
	--static \
	-nostartfiles \
	-L$(LIBOPENCM3)/lib \
	-Wl,--gc-sections \
	$(MFLAGS)

LDLIBS := \
	-lopencm3_stm32f1 \
	-Wl,--start-group \
	-lc \
	-lgcc \
	-Wl,--end-group

# FIXME
LDLIBS += $(LIBPRIM_LIBS)

%.d: %.c
	$(GCC) $(CFLAGS) -M -MG -MT $(@:.d=.o) $<

%.o: %.c $(MAKEFILE)
	$(GCC) $(CFLAGS) -DFIRMWARE=\"$(notdir $*)\" -c $$(readlink -f $<) -o $@

%.core.elf: %.o lib.a core.ld stm32f1.ld ../registers_stm32f103.o 
	$(GCC) $(LDFLAGS) -Tcore.ld -Wl,-Map=$*.map -o $@ $*.o ../registers_stm32f103.o lib.a $(LDLIBS)

%.config.elf: %.o lib.a core.ld stm32f1.ld
	$(GCC) $(LDFLAGS) -Tconfig.ld -Wl,-Map=$*.map -o $@ $*.o lib.a $(LDLIBS)

lib.a: $(O_LIB)
	ar -r $@ $(O_LIB)



TEST_RAM_O := \
	bl_hyministm32v.core.bl_ram.o \




# Create rule for each separate linker script $(APP).ld
# Device list with RAM specs here:
# http://www.st.com/web/en/catalog/mmc/FM141/SC1169/SS1576
APP := x8
-include app.mk
APP := xC
-include app.mk
APP := xD
-include app.mk
APP :=


# Support for exo:make_push

# This is an overestimate.  The next rule will prune.
push.list: $(ELF)
	echo $(ELF) >$@

# Generate actual files to avoid pushes when not necessary.
%.elf.push: %.elf
	[ ! -z "$$DST" ]
	echo "gdbstub push_change $$(readlink -f .) $<" | socat - TCP:$$DST
	touch $@

# This is needed to avoid pointless relinking of the .elf files.  See
# elf deps list in app.mk
.SECONDARY:
