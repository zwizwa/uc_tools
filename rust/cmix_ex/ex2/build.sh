set -x
cargo build --target thumbv7em-none-eabihf
cargo rustc -- -C link-arg=-nostartfiles

