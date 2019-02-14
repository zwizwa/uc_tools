
# $(APP).d corresponds to the linker script with target-specific details (i.e. memory layout).
%.$(APP).elf: LOCAL_APP := $(APP)
%.$(APP).elf: %.o lib.a $(APP).ld $(UC_TOOLS)/registers_stm32f103.o
	$(GCC) $(LDFLAGS) -T$(LOCAL_APP).ld -Wl,-Map=$*.$(LOCAL_APP).map -o $*.$(LOCAL_APP).elf $*.o $(UC_TOOLS)/registers_stm32f103.o lib.a $(LDLIBS)

