core.f103.ld    STM32F103 boot loader core
core.f407.ld    STM32F407 boot loader core

( FIXME: Currently not operational. )
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
