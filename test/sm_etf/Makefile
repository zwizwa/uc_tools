all: test.elf

.PHONY: clean
clean:
	rm -f *.o *.lib *.elf

O_LIB := sm_etf.o

HERE := $(shell readlink -f . )
CFLAGS := \
	-g \
	-I $(HERE) \
	-I $(HERE)/../../ \
	-I $(HERE)/../../gdb \

lib.a: $(O_LIB)
	ar -r $@ $(O_LIB)

%.elf: %.o lib.a
	gcc -o $@ $< lib.a

%.o: %.c Makefile
	gcc $(CFLAGS) -o $@ -c $<

push.list: all
	touch $@

