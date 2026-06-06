# A note on buildFHSUserEnv
# See build.sh and keep in mind there are 2 levels of wrapping involved.
# - This .nix file creates a shell environment that contains a ipxe-fhs script
# - The ipxe-fhs script uses bwrap to create the FHS environment

# The ipxe build system expects to be able to build an executable and
# run it and this makes a FHS assumption.

let
  nixpkgs = import ../nix/nixpkgs.nix;
in
with import nixpkgs { };
mkShell {
  buildInputs = [
    (pkgs.buildFHSUserEnv {
      name = "ipxe-fhs";
      targetPkgs = pkgs: with pkgs; [
        gcc gnumake binutils perl xz xz.dev cdrkit syslinux lzma xz
      ];
    })
  ];
}

