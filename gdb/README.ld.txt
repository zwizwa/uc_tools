config.ld       Configuration block expected by bootloader
core.ld         STM32F103 boot loader core
core_f4.ld      STM32F407 boot loader core
data.ld         Embedded Flash Data partition

Memory configs for submodels
- STM32F103
x8.ld           
xC.ld
xD.ld
- STM32F407
TODO

Main .ld files for each platform, derived from libopencm3 .ld files:

stm32f1.ld      
stm32f4.ld
