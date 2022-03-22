all:
	cargo build
	if [ ! -z "$$ELF" ]; then ln -sf target/debug/test_async "$$ELF"; fi
