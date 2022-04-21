
// Your application will generally be no_std, MnemOS does not currently provide
// a version of the standard library
#![no_std]

// Your application will generally need the no_main attribute (similar to
// embedded rust programs) - as we do not use Rust's built-in main function,
// and instead use `entry() -> !`
#![no_main]

/// Even if you use no system calls, you should probably include the
/// userspace library as shown here, to ensure the panic handler (and
/// other necessary components) are linked in.
///
/// Note: Although the crate name is `mnemos-userspace`, it is imported
/// as just `userspace`.
use userspace as _;

/// Though in this example, we will use a couple of system calls for
/// demonstration purposes.
use userspace::common::porcelain::{serial, time};

/// The entry point function MUST:
///
/// * Be declared with the #[no_mangle] attribute
/// * Must never return
#[no_mangle]
fn entry() -> ! {
    loop {
        // Note: You probably should handle errors, but this is a demo.
        serial::write_port(0, b"Hello, world!\r\n").ok();
        time::sleep_micros(500_000).ok();
    }
}
