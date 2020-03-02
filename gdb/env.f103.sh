# If this is not defined, assume it is current working directory.
[ -z "$UC_TOOLS" ] && UC_TOOLS=..

. $UC_TOOLS/gdb/env.common.sh

MFLAGS=" \
-mthumb \
-mcpu=cortex-m3 \
-msoft-float \
-mfix-cortex-m3-ldrd \
" 

CPPFLAGS="$CPPFLAGS_COMMON"
CFLAGS="$CFLAGS_COMMON $CPPFLAGS $MFLAGS -DSTM32F1"
LDFLAGS="$LDFLAGS_COMMON $MFLAGS"
LDLIBS="$LDLIBS_COMMON -lopencm3_stm32f1"
ELF="$ELF_COMMON"

ELF="
bl_hyministm32v.core.f103.elf
bl_hyministm32v_autostart.core.f103.elf
bl_c8t6.core.f103.elf
bl_c8t6_usb_pullup.core.f103.elf
relay_board.x8.f103.elf
lab_board.x8.f103.elf
gpio_etf.x8.f103.elf
wavegen.x8.f103.elf
usbisr.x8.f103.elf
etf_test.x8.f103.elf
echo_test.x8.f103.elf
doodle.x8.f103.elf
ethernet.x8.f103.elf
dht11.x8.f103.elf
switch.x8.f103.elf
slipstub_example.x8.f103.elf
rawslip.x8.f103.elf
uart_router.x8.f103.elf
spi_test.x8.f103.elf
host.x8.f103.elf
forth_app.x8.f103.elf
os.x8.f103.elf
"

O_SYSTEM=$UC_TOOLS/gdb/registers_stm32f103.f103.o
