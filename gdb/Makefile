# This is a wrapper for minimal do, not supporting incremental builds.
# For the full version of redo, visit http://github.com/apenwarr/redo
.PHONY: all clean clean_deps
all: .stamp
clean:
	rm -f *.o *.a *.elf *.beam *.map .stamp
clean_deps: clean
	rm -rf ../../libopencm3 ../../gcc-arm-none-eabi-4_8-2014q2/
.stamp:
	./install-deps.sh
	./do -c 2>&1 | grep 'do '
	touch $@
