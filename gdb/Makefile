C_LIB := \
	cdcacm_desc.c \
	gdbstub.c \
	memory.c \
	rsp_packet.c \
	vector.c \
	bootloader.c \
	hw_bootloader.c \
	../tools.c \

C_OTHER := \
	bl_hyministm32v.c \
	bl_hyministm32v_autostart.c \
	bl_c8t6.c \

ELF := \
	bl_hyministm32v.core.elf \
	bl_hyministm32v_autostart.core.elf \
	bl_c8t6.core.elf \

O_LIB := $(C_LIB:.c=.o)

all: $(ELF)

clean:
	rm -rf *~ *.o *.d *.dasm *.a *.elf

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

%.d: %.c
	$(GCC) $(CFLAGS) -M -MG -MT $(@:.d=.o) $<

%.o: %.c $(MAKEFILE)
	$(GCC) $(CFLAGS) -c $$(readlink -f $<) -o $@

%.core.elf: %.o lib.a core.ld stm32f1.ld ../registers_stm32f103.o 
	$(GCC) $(LDFLAGS) -Tcore.ld -Wl,-Map=$*.map -o $@ $*.o ../registers_stm32f103.o lib.a $(LDLIBS)

%.config.elf: %.o lib.a core.ld stm32f1.ld
	$(GCC) $(LDFLAGS) -Tconfig.ld -Wl,-Map=$*.map -o $@ $*.o lib.a $(LDLIBS)

lib.a: $(O_LIB)
	ar -r $@ $(O_LIB)



TEST_RAM_O := \
	bl_hyministm32v.core.bl_ram.o \


