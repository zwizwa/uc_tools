printf "*** executing flash.gdb\n"
printf "*** loading elf\n"
file build/zephyr/zephyr.elf
printf "*** connecting to programmer\n"
target remote 10.1.3.12:3301
printf "*** running\n"
continue
