#![no_std]  // don't link the Rust standard library
extern crate panic_halt;

#[no_mangle]
pub extern fn add1(i:u32) -> u32 {
    i+1
}
