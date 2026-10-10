# Dual function (see make.sh)
# - for (cached-)nix-shell
# - explicit env tool chain tree

# https://claude.ai/chat/02adce80-7401-4874-bf0e-0b11437d3407


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
