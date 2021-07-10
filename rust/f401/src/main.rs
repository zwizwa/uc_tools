/* Example for Black Pill board derived from the F103 example.
   https://stm32-base.org/boards/STM32F411CEU6-WeAct-Black-Pill-V2.0.html
   That says user led is PC13.

   The chip is STM32F401CCU6
            PM
   P) package: C=48 pin
   M) memory:  C=256K/64K

   Note that F401 GPIO peripheral (F2 and F4) is not the same as the F1.
   Also RCC is different.

   https://docs.rs/stm32f4/
   STM RM0368 Reference manual

   Register writes in rust embedded seem to follow the same pattern: a
   .write() method takes a callback which is a reference to a writer
   structure.

*/


#![no_std]
#![no_main]

extern crate cortex_m_rt;
extern crate stm32f4;
extern crate panic_halt;

use cortex_m_rt::entry;
use stm32f4::stm32f401;

#[entry]
fn main() -> ! {

    // get handles to the hardware
    let peripherals = stm32f401::Peripherals::take().unwrap();
    let gpioc = &peripherals.GPIOC;
    // MODER: mode register
    gpioc.moder.write(|w| w.moder13().output());
    loop{
        // BSRR: bit set/reset register.
        gpioc.bsrr.write(|w| w.bs13().set_bit()); cortex_m::asm::delay(2000000);
        gpioc.bsrr.write(|w| w.br13().set_bit()); cortex_m::asm::delay(2000000);
    }
}

