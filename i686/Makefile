NASM = nasm

# Use CTRL-ALT-G to escape a qemu window when it takes focus.

# QEMU = qemu-system-i386 -nographic -no-reboot -m 16
# QEMU = qemu-system-i386 -m 16 -display gtk,zoom-to-fit=on -serial stdio
QEMU = qemu-system-i386 -m 64 -display sdl,gl=on -serial stdio -device isa-debug-exit,iobase=0xf4,iosize=0x04
# QEMU = qemu-kvm -m 16
GCC = i686-elf-gcc -ffreestanding -nostdlib -m32 -march=i486
LD = i686-elf-ld
OBJCOPY = i686-elf-objcopy
OBJDUMP = i686-elf-objdump

UC_TOOLS=..

# sqlite amalgamation: ./configure && make sqlite3.c
SQLITE = ../../sqlite/bld
SQLITE_FLAGS = \
	-DSQLITE_OS_OTHER=1 \
	-DSQLITE_THREADSAFE=0 \
	-DSQLITE_ZERO_MALLOC \
	-DSQLITE_ENABLE_MEMSYS5 \
	-DSQLITE_OMIT_AUTOINIT \
	-DSQLITE_TEMP_STORE=3 \
	-DSQLITE_DEFAULT_MEMSTATUS=0 \
	-DSQLITE_DQS=0 \
	-DSQLITE_OMIT_LOAD_EXTENSION \
	-DSQLITE_OMIT_WAL \
	-DSQLITE_OMIT_SHARED_CACHE \
	-DSQLITE_OMIT_DEPRECATED \
	-DSQLITE_OMIT_PROGRESS_CALLBACK \
	-DSQLITE_OMIT_AUTHORIZATION \
	-DSQLITE_OMIT_LOCALTIME \
	-DSQLITE_OMIT_TRACE \
	-DSQLITE_OMIT_JSON \
	-DSQLITE_OMIT_COMPLETE \
	-DSQLITE_OMIT_GET_TABLE \
	-DSQLITE_OMIT_UTF16 \
	-DSQLITE_OMIT_DECLTYPE \
	-DSQLITE_OMIT_DESERIALIZE \
	-DSQLITE_OMIT_SEH \
	-DSQLITE_UNTESTABLE \

CFLAGS = \
	-Wall \
	-I$(UC_TOOLS) \
	-I$(SQLITE) \
	-DVERSION=\"$(shell ./version.sh)\" \
	-Os \


.PHONY: all clean run

all: kernel.nbp kernel.dsk kernel.dasm trampoline.blk kernel.3if sqlite_plugin.bin

DEPS := $(shell find -name '*.d')
-include $(DEPS)

kernel.nbp: boot.blk kernel.bin
	cat boot.blk kernel.bin >$@

kernel.3if: trampoline.blk kernel.bin
	cat trampoline.blk kernel.bin >$@



#  512 sector size
# 9216 track size (18 sectors)
kernel.dsk: boot.dsk kernel.bin
	cat boot.dsk kernel.bin >$@
	truncate -s 1474560 $@  # qemu needs a real floppy size
	truncate -s %512 $@
	ls -l $@

clean:
	rm -f *.bin *.elf boot.dsk boot.blk test.nbp kernel.dsk kernel.nbp *~ *.dasm *.o


# 512 byte boot sectors for NBP and floppy, loaded at 0x7C00.  This
# will do any necessary loading (for floppy) and switch to protected
# mode before jumping into the C kernel.
boot.blk: boot.asm
	$(NASM) -f bin -o $@ $<

test.nbp: boot.asm
	$(NASM) -dTESTKERNEL -f bin -o $@ $<

trampoline.blk: trampoline.asm
	$(NASM) -f bin -o $@ $<

boot.dsk: boot.asm
	$(NASM) -dFLOPPY -f bin -o $@ $<


# Kernel is C code, loaded at 0x7E00 after the boot sector.
%.o: %.c
	$(GCC) $(CFLAGS) -MD -MF $(patsubst %.o,%.d,$@) -c $< -o $@

sqlite3.o: $(SQLITE)/sqlite3.c Makefile
	$(GCC) -Os $(SQLITE_FLAGS) -c $< -o $@

kernel.elf: kernel.o kernel.ld
	$(LD) -T kernel.ld -o $@ kernel.o $(shell $(GCC) -print-libgcc-file-name)

# Plugins are linked against the kernel they get loaded into, and
# check its version string at load.  See plugin.h
sqlite_plugin.o: sqlite_plugin.c kernel.bin
	$(GCC) $(CFLAGS) -DKERNEL_VERSION=\"$$(grep -a -o -E '[0-9]{8}-[0-9]{4}' kernel.bin | head -1)\" -MD -MF sqlite_plugin.d -c $< -o $@

sqlite_plugin.elf: sqlite_plugin.o sqlite3.o plugin.ld kernel.elf
	$(LD) -T plugin.ld -R kernel.elf -o $@ sqlite_plugin.o sqlite3.o $(shell $(GCC) -print-libgcc-file-name)

%.elf: %.o kernel.ld
	$(LD) -T kernel.ld -o $@ $<

%.bin: %.elf
	$(OBJCOPY) -O binary $< $@

%.dasm: %.elf
	rm -f $@
	$(OBJDUMP) -d $< >$@.tmp
	mv $@.tmp $@

%.dasm: %.blk Makefile
	rm -f $@
	$(OBJDUMP) -D -b binary -m i386 $< >$@.tmp
	mv $@.tmp $@
	cat $@

# QEMU floppy options.  Turn off locking to allow multiple instances
# to use the same image.  Kernel and iPXE will not write to the
# floppy.
FLOPPY=format=raw,if=floppy,file.locking=off

# Test in QEMU loading as a floppy.  
qemu_dsk: kernel.dsk
	@echo "WARNING: floppy image does not reload on qemu reboot"
	@echo "WARNING: use qemu_nbp instead"
	$(QEMU) -drive file=kernel.dsk,$(FLOPPY) -boot a -netdev user,id=net0 -device rtl8139,netdev=net0

# Test in QEMU using ipxe and user mode networking with internal tftp
# server.  This is a bit slow due to the DHCP step.
#qemu_nbp_slow: kernel.nbp
#	$(QEMU) -boot n -netdev user,id=net0,tftp=.,bootfile=$< -device virtio-net,netdev=net0



# Faster boot with ipxe boot floppy compiled with static ip and hard coded boot file.
# See /i/tom/git/ipxe/qemu.sh
# Using rtl8139 instead of virtio-net
qemu_nbp: kernel.nbp ipxe-qemu.dsk
	$(QEMU) -drive file=ipxe-qemu.dsk,$(FLOPPY) -boot a -netdev user,id=net0,tftp=. -device rtl8139,netdev=net0  || echo -n


# Linux process on vfio-pci, see hw_vfio.h
vfio_%.elf: vfio_%.c hw_vfio.h
	env/bin/gcc -m32 -Wall -Os -I.. -o $@ $<

# Push it to the boot server.
NBP = kernel.nbp test.nbp
TFTPBOOT = root@rackhub:/etc/net/tftpboot/
push: $(NBP)
	scp $(NBP) $(TFTPBOOT)

# Then boot it in qemu
qemu_br1: kernel.nbp ipxe-qemu-br1.dsk
	scp $< $(TFTPBOOT)
	sudo $(QEMU) -drive file=ipxe-qemu-br1.dsk,format=raw,if=floppy,file.locking=off -boot a -netdev bridge,id=net0,br=br1 -device rtl8139,netdev=net0

# Similar, but load tftp://10.1.3.1/menu.ipxe for testing other boot files
qemu_br1_menu: kernel.nbp ipxe-qemu-br1-menu.dsk
	scp $< $(TFTPBOOT)
	sudo $(QEMU) -drive file=ipxe-qemu-br1-menu.dsk,format=raw,if=floppy,file.locking=off -boot a -netdev bridge,id=net0,br=br1 -device rtl8139,netdev=net0


# USB floppy drive
%.dd: %.dsk
	sudo dd if=$< of=/dev/disk/by-id/usb-TEAC_TEAC_FD-05PUB

# Embed an .ipxe boot file in a boot disk for the emulated rtl8139
# Note that these need to have an ipxe source tree linked into ../ipxe/src
ipxe-%.dsk: %.ipxe
	../ipxe/build.sh rtl8139.dsk $<
	mv ../ipxe/src/bin/rtl8139.dsk $@

# The host.nix now has env and shell targets. The former is a link
# that contains a bin directory that can be added to path before
# invoking make.  See make.sh
env: host.nix
	nix-build host.nix -A env -o env



# gotek:
# sudo /i/tom/floppies/gotek.sh kernel.dsk /dev/sdf 4
