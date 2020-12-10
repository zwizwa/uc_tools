# For general installation instructions, see
# ../install.sh
# https://rust-embedded.github.io/cortex-m-quickstart/cortex_m_quickstart/

# We use cargo to build, but provide this wrapper makefile for do.erl

# This is now set as an environment variable.
# ELF=bluepill.f103.elf

.PHONY: all
all:
	cargo build --release
	[ ! -z "$$ELF" ] && ln -f target/thumbv7m-none-eabi/release/bluepill "$$ELF" # && arm-none-eabi-objdump -d $$ELF

%.readelf: %.elf
	readelf -a $< >$@.tmp
	mv $@.tmp $@

.PHONY: clean
clean:
	rm -rf target *~ *.elf *.dasm *.readelf

%.dasm: %.elf
	arm-none-eabi-objdump -d $< >$@.tmp
	mv $@.tmp $@





