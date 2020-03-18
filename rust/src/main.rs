#![no_std]
#![no_main]

//extern crate cortex_m;
//#[macro_use(entry, exception)]
extern crate cortex_m_rt;
extern crate stm32f1;
extern crate panic_halt;

use cortex_m_rt::entry;
use stm32f1::stm32f103;


// extern crate panic_semihosting;

//use core::panic::PanicInfo;
// #[panic_handler]
// fn panic(_info: &PanicInfo) -> ! {
//     loop {}
// }

// use cortex_m::asm;
// use rt::ExceptionFrame;

entry!(main);
fn main() -> ! {
    // loop { asm::bkpt(); }

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

// exception!(HardFault, hard_fault);
// fn hard_fault(ef: &ExceptionFrame) -> ! {
//     panic!("HardFault at {:#?}", ef);
// }

// exception!(*, default_handler);
// fn default_handler(irqn: i16) {
//     panic!("Unhandled exception (IRQn = {})", irqn);
// }
