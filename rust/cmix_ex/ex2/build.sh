set -x
# Build the static library using cargo.
# Release optimizes away some panic references.
cargo build --release
# Link it
rm -f test
gcc -o test test.c -L ./target/release -lcmix_ex2 -Wl,--gc-sections
./test


