NASM = nasm
# QEMU = qemu-system-i386 -nographic -no-reboot -m 16
QEMU = qemu-system-i386 -m 16
# QEMU = qemu-kvm -m 16
GCC = i686-elf-gcc -ffreestanding -nostdlib -m32
LD = i686-elf-ld
OBJCOPY = i686-elf-objcopy
OBJDUMP = i686-elf-objdump

UC_TOOLS=..

CFLAGS = \
	-I$(UC_TOOLS)


.PHONY: all clean run

all: zpxe.nbp

zpxe.nbp: boot.nbp kernel.bin
	cat boot.nbp kernel.bin >$@

clean:
	rm -f *.bin *.elf zpxe.dsk zpxe.nbp *~ *.dasm *.o


# 512 byte boot sectors for NBP and floppy, loaded at 0x7C00.  This
# will do any necessary loading (for floppy) and switch to protected
# mode before jumping into the C kernel.
boot.nbp: boot.asm
	$(NASM) -f bin -o $@ $<
boot.dsk: boot.asm
	$(NASM) -dFLOPPY -f bin -o $@ $<


# Kernel is C code, loaded at 0x7E00 after the boot sector.
%.o: %.c
	$(GCC) $(CFLAGS) -Os -c $< -o $@

%.elf: %.o kernel.ld
	$(LD) -T kernel.ld -o $@ $<

%.bin: %.elf
	$(OBJCOPY) -O binary $< $@

%.dasm: %.elf
	rm -f $@
	$(OBJDUMP) -d $< >$@.tmp
	mv $@.tmp $@

# Test in QEMU loading as a floppy.  This will need a loader if it
# gets bigger than the boot sector.
qemu_dsk: zpxe.dsk
	$(QEMU) -drive file=$<,format=raw,if=floppy -boot a

# Test in QEMU using ipxe and internal tftp server.  This is a bit
# slow due to the DHCP step.
qemu_nbp_slow: zpxe.nbp
	$(QEMU) -boot n -netdev user,id=net0,tftp=.,bootfile=$< -device virtio-net,netdev=net0

# Faster boot with ipxe boot floppy compiled with static ip and hard coded boot file.
# See /i/tom/git/ipxe/qemu.sh
DSK=ipxe-qemu.dsk
qemu_nbp: zpxe.nbp
	$(QEMU) -drive file=$(DSK),format=raw,if=floppy -boot a -netdev user,id=net0,tftp=. -device virtio-net,netdev=net0


# Push it to the boot server.
push: zpxe.nbp
	scp $< root@rackhub:/etc/net/tftpboot/

