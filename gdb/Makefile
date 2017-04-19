C := \
	cdcacm_desc.c \
	gdbstub.c \
	memory.c \
	rsp_packet.c \
	vector.c \

O := $(C:.c=.o)

all: $(O)

clean:
	rm -rf *~ *.o *.d

# FIXME: transition only
TOP := /home/build/src/gw_src/modules/
TPF := $(TOP)/toolchain/gcc-arm-none-eabi-4_8-2014q2/bin/arm-none-eabi
LIBOPENCM3 := $(TOP)/libopencm3

GCC      := $(TPF)-gcc
OBJDUMP  := $(TPF)-objdump
OBJCOPY  := $(TPF)-objcopy
READELF  := $(TPF)-readelf
HOST_GCC := gcc

MAKEFILE := Makefile

# Machine flags needed both for compiling and linking.
MFLAGS := \
	-mthumb \
	-mcpu=cortex-m3 \
	-msoft-float \
	-mfix-cortex-m3-ldrd

CPPFLAGS := \
	-I.. \
	-I$(LIBOPENCM3)/include \

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

%.o: %.c $(MAKEFILE)
	$(GCC) $(CFLAGS) -c $$(readlink -f $<) -o $@

