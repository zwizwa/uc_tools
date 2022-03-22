.PHONY: all
all:
	cargo build --release
	if [ ! -z "$$ELF" ]; then ln -f target/thumbv7m-none-eabi/release/bluepill "$$ELF"; fi

%.readelf: %.elf
	readelf -a $< >$@.tmp
	mv $@.tmp $@

.PHONY: clean
clean:
	rm -rf target *~ *.elf *.dasm *.readelf

%.dasm: %.elf
	arm-none-eabi-objdump -d $< >$@.tmp
	mv $@.tmp $@





