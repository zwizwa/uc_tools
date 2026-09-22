# let
#   nixpkgs = import ../nix/nixpkgs.nix;
# in
# with import nixpkgs { };

# let i686 = pkgsCross.i686-embedded.buildPackages;

# in mkShell {
#   buildInputs = [
#     nasm
#     i686.gcc        # i686-elf-gcc
#     i686.binutils   # i686-elf-ld
#   ];
# }

# https://claude.ai/chat/02adce80-7401-4874-bf0e-0b11437d3407

# I want to move towards using ~/.result links that are built
# automatically, and a build that doesn't use cached-nix-shell anymore
# but the paths from the result instead.

let
  pkgs = import (import ../nix/nixpkgs.nix) { };
  i686 = pkgs.pkgsCross.i686-embedded.buildPackages;

  tools = with pkgs; [
    coreutils bash findutils gnumake
    nasm
    i686.gcc        # i686-elf-gcc
    i686.binutils   # i686-elf-ld
  ];
in {
  # development
  shell = pkgs.mkShell { packages = tools; };

  # deployable: result/bin with all tools
  env = pkgs.buildEnv {
    name = "i686-toolchain";
    paths = tools;
    pathsToLink = [ "/bin" ];
  };
}
