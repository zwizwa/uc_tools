#![no_std]
#![no_main]

extern crate cortex_m;
extern crate cortex_m_rt;
extern crate cortex_m_semihosting;
extern crate stm32f1;
extern crate panic_semihosting;

use cortex_m_rt::entry;
use cortex_m_semihosting::{hprintln};

use stm32f1::stm32f103;



entry!(main);
fn main() -> ! {

    hprintln!("Hello, world!").unwrap();

    // get handles to the hardware
    let peripherals = stm32f103::Peripherals::take().unwrap();
    let gpioc = &peripherals.GPIOC;
    let rcc = &peripherals.RCC;

    // enable the GPIO clock for IO port C
    rcc.apb2enr.write(|w| w.iopcen().set_bit());
    gpioc.crh.write(|w| unsafe{
        w.mode13().bits(0b11);
        w.cnf13().bits(0b00)
    });

    loop{
        gpioc.bsrr.write(|w| w.bs13().set_bit());
        cortex_m::asm::delay(2000000);
        gpioc.brr.write(|w| w.br13().set_bit());
        cortex_m::asm::delay(2000000);
    }
}

