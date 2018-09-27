# See https://rust-embedded.github.io/cortex-m-quickstart/cortex_m_quickstart/
# Idempotent install script: install.sh

.PHONY: all
all: bluepill.elf bluepill.dasm bluepill.readelf

target/thumbv7m-none-eabi/release/bluepill:
	cargo build --release

bluepill.elf: target/thumbv7m-none-eabi/release/bluepill
	rm -rf $@
	ln -s $< $@

%.readelf: %.elf
	readelf -a $< >$@.tmp
	mv $@.tmp $@

.PHONY: clean
clean:
	rm -rf target *~ *.elf *.dasm *.readelf

%.dasm: %.elf
	arm-none-eabi-objdump -d $< >$@.tmp
	mv $@.tmp $@





