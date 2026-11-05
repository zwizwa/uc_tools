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
    gcc_multi       # gcc -m32 for vfio_*.elf
  ];
in {
  # development
  shell = pkgs.mkShell { packages = tools; };

  # deployable: result/bin with all tools
  #
  # PATH is the whole interface, which is what makes this usable
  # without nix: see with-env.sh.  Note this is deliberately *not* the
  # 'shell' above serialized to a file.  mkShell treats 'packages' as
  # nativeBuildInputs, so bash and gnumake contribute -isystem entries
  # for their dev outputs ahead of the compiler's own include path,
  # which has no business in a -ffreestanding -nostdlib cross build.
  env = pkgs.buildEnv {
    name = "i686-toolchain";
    paths = tools;
    pathsToLink = [ "/bin" ];
  };
}
