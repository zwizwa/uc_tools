# See https://rust-embedded.github.io/cortex-m-quickstart/cortex_m_quickstart/
# Idempotent install script: install.sh

BN=blackpill
ELF=$(BN).f407.elf
BIN=$(BN).f407.bin

.PHONY: all
all:
	cargo build --release
	ln -sf target/thumbv7em-none-eabihf/release/blackpill $(ELF)
	arm-none-eabi-objcopy -O binary $(ELF) $(BIN)
#	md5sum $(ELF)
#	arm-none-eabi-objdump -d $(ELF)

%.readelf: %.elf
	readelf -a $< >$@.tmp
	mv $@.tmp $@

.PHONY: clean
clean:
	rm -rf target *~ *.elf *.dasm *.readelf

%.dasm: %.elf
	arm-none-eabi-objdump -d $< >$@.tmp
	mv $@.tmp $@

.PHONY: flash
flash:
	dfu-util -a 0 --dfuse-address 0x08000000 -D blackpill.f407.bin 
