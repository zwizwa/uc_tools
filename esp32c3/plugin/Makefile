# Tool prefix
TPF := riscv32-esp-elf-

GCC     := $(TPF)gcc
OBJDUMP := $(TPF)objdump
OBJCOPY := $(TPF)objcopy

CFLAGS := \


.PHONY: all clean

all: test.o test.iram.bin test.dram.bin

clean:
	rm -f *.elf *.o *.bin *~

clean: link_clean
	rm -f *.o *~

link_clean:
	rm -f *.bin *.ld *.elf meminfo.sh 

%.o: %.c
	$(GCC) -Os -o $@ -c $<
	$(OBJDUMP) -x $@
	$(OBJDUMP) -d $@

%.elf: %.o sram.ld
	$(GCC) --static -nostartfiles -Tsram.ld -o $@ $<
	$(OBJDUMP) -x $@
	$(OBJDUMP) -d $@

%.iram.bin: %.elf
	$(OBJCOPY) -O binary --only-section=.iram $< $@
	hexdump -C $@

%.dram.bin: %.elf
	$(OBJCOPY) -O binary --only-section=.dram $< $@
	hexdump -C $@

meminfo.sh: cmd.sh
	./cmd.sh meminfo >$@.tmp
	mv $@.tmp $@

%.ld: %.ld.sh meminfo.sh
	sh $< >$@.tmp
	mv $@.tmp $@


