NASM = nasm
# QEMU = qemu-system-i386 -nographic -no-reboot -m 16
QEMU = qemu-system-i386 -m 16 -display gtk,zoom-to-fit=on
# QEMU = qemu-kvm -m 16
GCC = i686-elf-gcc -ffreestanding -nostdlib -m32
LD = i686-elf-ld
OBJCOPY = i686-elf-objcopy
OBJDUMP = i686-elf-objdump

UC_TOOLS=..

CFLAGS = \
	-I$(UC_TOOLS)


.PHONY: all clean run

all: kernel.nbp kernel.dsk kernel.dasm

kernel.nbp: boot.nbp kernel.bin
	cat boot.nbp kernel.bin >$@

kernel.dsk: boot.dsk kernel.bin
	cat boot.dsk kernel.bin >$@
	truncate -s %512 $@

clean:
	rm -f *.bin *.elf boot.dsk boot.nbp test.nbp kernel.dsk kernel.nbp *~ *.dasm *.o


# 512 byte boot sectors for NBP and floppy, loaded at 0x7C00.  This
# will do any necessary loading (for floppy) and switch to protected
# mode before jumping into the C kernel.
boot.nbp: boot.asm
	$(NASM) -f bin -o $@ $<

test.nbp: boot.asm
	$(NASM) -dTESTKERNEL -f bin -o $@ $<

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

kernel.o: hw_i686.h text_console.h $(UC_TOOLS)/line_editor.h

# Test in QEMU loading as a floppy.  This will need a loader if it
# gets bigger than the boot sector.
qemu_dsk: kernel.dsk
	@echo "WARNING: floppy image does not reload on qemu reboot"
	@echo "WARNING: use qemu_nbp instead"
	$(QEMU) -drive file=$<,format=raw,if=floppy -boot a

# Test in QEMU using ipxe and user mode networking with internal tftp
# server.  This is a bit slow due to the DHCP step.
#qemu_nbp_slow: kernel.nbp
#	$(QEMU) -boot n -netdev user,id=net0,tftp=.,bootfile=$< -device virtio-net,netdev=net0




# Faster boot with ipxe boot floppy compiled with static ip and hard coded boot file.
# See /i/tom/git/ipxe/qemu.sh
# Copy the boot floppy image to avoid locks on the NFS share
# Using rtl8139 instead of virtio-net
DSK=ipxe-qemu.dsk
qemu_nbp: kernel.nbp
	cp -a $(DSK) /tmp/$(DSK)
	$(QEMU) -drive file=/tmp/$(DSK),format=raw,if=floppy -boot a -netdev user,id=net0,tftp=. -device rtl8139,netdev=net0



# Push it to the boot server.
NBP = kernel.nbp test.nbp
push: $(NBP)
	scp $(NBP) root@rackhub:/etc/net/tftpboot/

# Then boot it in qemu
qemu_br1: push
#	$(QEMU) -boot n -netdev bridge,id=net0,br=br1 -device rtl8139,netdev=net0
	sudo $(QEMU) -drive file=ipxe-qemu-br1.dsk,format=raw,if=floppy -boot a -netdev bridge,id=net0,br=br1 -device rtl8139,netdev=net0


# USB floppy drive
%.dd: %.dsk
	sudo dd if=$< of=/dev/disk/by-id/usb-TEAC_TEAC_FD-05PUB

# gotek:
# sudo /i/tom/floppies/gotek.sh kernel.dsk /dev/sdf 4
