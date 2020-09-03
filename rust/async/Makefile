all:
	cargo build
	[ ! -z "$$ELF" ] && ln -sf target/debug/test_async "$$ELF"
