# ---- Configuration

TPF=../../gcc-arm-none-eabi-4_8-2014q2/bin/arm-none-eabi
LIBOPENCM3=../../libopencm3
UC_TOOLS=..
VERSION=$(git rev-parse HEAD)

# ---- Implementation

GCC=${TPF}-gcc
OBJDUMP=${TPF}-objdump
OBJCOPY=${TPF}-objcopy
READELF=${TPF}-readelf

MFLAGS=" \
-mthumb \
-mcpu=cortex-m3 \
-msoft-float \
-mfix-cortex-m3-ldrd \
" 

CPPFLAGS="\
-I.. \
-I$UC_TOOLS/include \
-I$LIBOPENCM3/include \
"

CFLAGS=" \
-std=c99 \
-fno-common \
-ffunction-sections \
-fdata-sections \
-MD \
-DSTM32F1 \
-Wall \
-Werror \
-Wno-format \
-Wno-attributes \
-Wno-multichar \
-g \
-Os \
$CPPFLAGS \
$MFLAGS \
"

LDFLAGS=" \
-g \
--static \
-nostartfiles \
-L$LIBOPENCM3/lib \
-Wl,--gc-sections \
$MFLAGS \
"

LDLIBS="\
-lopencm3_stm32f1 \
-Wl,--start-group \
-lc \
-lgcc \
-Wl,--end-group \
"

ELF="
bl_hyministm32v.core.elf
bl_hyministm32v_autostart.core.elf
bl_c8t6.core.elf
relay_board.x8.elf
gpio_etf.x8.elf
wavegen.x8.elf
usbisr.x8.elf
etf_test.x8.elf
echo_test.x8.elf
"
