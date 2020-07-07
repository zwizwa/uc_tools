core.f103.ld    STM32F103 boot loader core
core.f407.ld    STM32F407 boot loader core ( FIXME: Currently not operational. )
config.ld       Configuration block expected by bootloader
data.ld         Embedded Flash Data partition

Memory configs for submodels
- STM32F103
x8.f103.ld           
xC.f103.ld
xD.f103.ld
- STM32F407
FIXME

Main .ld files for each platform, derived from libopencm3 .ld files:

stm32f1.ld      
stm32f4.ld


Note that there are also generator files for the .ld files.  See the
*.ld.sh scripts.  This was originally done to allow inlining of ld
files into a single file, to avoid path issues.

