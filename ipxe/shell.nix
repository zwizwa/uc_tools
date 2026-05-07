# { pkgs ? import <nixpkgs> {} }:
# (pkgs.buildFHSUserEnv {
#   name = "ipxe-build";
#   targetPkgs = pkgs: with pkgs; [
#     gcc gnumake binutils perl xz xz.dev cdrkit syslinux lzma xz
#   ];
# }).env

let
  nixpkgs = import ../i686/nixpkgs.nix;
in
with import nixpkgs { };

let i686 = pkgsCross.i686-embedded.buildPackages;
in
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

