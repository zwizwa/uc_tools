#![no_std]
#![no_main]

extern crate cortex_m;
extern crate cortex_m_rt;
extern crate cortex_m_semihosting;
extern crate stm32l4;
extern crate panic_semihosting;

use cortex_m_rt::entry;
use cortex_m_semihosting::{debug, hprintln};


#[entry]
fn main() -> ! {
    hprintln!("Hello, world!").unwrap();
    debug::exit(debug::EXIT_SUCCESS);
    loop {
    }
}
