# This is a wrapper for minimal do, not supporting incremental builds.
# For the full version of redo, visit http://github.com/apenwarr/redo
.PHONY: all clean
all: .stamp
clean:
	rm -f *.o *.a *.elf *.beam *.map .stamp
.stamp:
	./do -c 2>&1 | grep 'do '
	touch $@
