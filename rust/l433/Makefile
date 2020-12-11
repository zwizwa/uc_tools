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





