with import <nixpkgs> {};
mkShell {
  buildInputs = [
    rustup gcc qemu gcc rust-cbindgen probe-run bintools-unwrapped
    pkg-config udev rust-analyzer gcc-arm-embedded

    # keep this line if you use bash
    bashInteractive
  ];
}
