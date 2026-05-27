let
  nixpkgs = import ../nix/nixpkgs.nix;
in
with import nixpkgs { };

let i686 = pkgsCross.i686-embedded.buildPackages;

in mkShell {
  buildInputs = [
    nasm
    i686.gcc        # i686-elf-gcc
    i686.binutils   # i686-elf-ld
  ];
}


