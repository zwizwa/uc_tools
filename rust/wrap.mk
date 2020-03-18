# See https://rust-embedded.github.io/cortex-m-quickstart/cortex_m_quickstart/
# Idempotent install script: install.sh

ELF=bluepill.f103.elf

.PHONY: all
all:
	cargo build --release
	ln -sf target/thumbv7m-none-eabi/release/bluepill $(ELF)
#	md5sum $(ELF)
	arm-none-eabi-objdump -d $(ELF)

%.readelf: %.elf
	readelf -a $< >$@.tmp
	mv $@.tmp $@

.PHONY: clean
clean:
	rm -rf target *~ *.elf *.dasm *.readelf

%.dasm: %.elf
	arm-none-eabi-objdump -d $< >$@.tmp
	mv $@.tmp $@





