# See https://rust-embedded.github.io/cortex-m-quickstart/cortex_m_quickstart/
# Idempotent install script: install.sh

.PHONY: all
all:
	cargo build --release
	readelf -a target/thumbv7m-none-eabi/release/bluepill

.PHONY: clean
clean:
	rm -rf target *~




