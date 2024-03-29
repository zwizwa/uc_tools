## Dependencies managed as a nix flake.
## See: https://www.tweag.io/blog/2020-05-25-flakes/

{
  description = "uc_tools";
  # Unstable snapshot.
  inputs.nixpkgs.url = github:NixOS/nixpkgs/85f1ba3e51676fa8cc604a3d863d729026a6b8eb;
  inputs.libopencm3 = ./libopencm3.nix;

  outputs = { self, nixpkgs }:
    let pkgs = nixpkgs.legacyPackages.x86_64-linux;
    in
  {
    packages.x86_64-linux.default =
      with import nixpkgs { system = "x86_64-linux"; };
      stdenv.mkDerivation {
        name = "uc_tools";
        src = self;
        buildInputs = [ pkgs.gcc-arm-embedded ];
        LIBOPENCM3 = libopencm3;

        # Tool PreFix.  See uc_tools/gdb/env.common.sh
        TPF = "${gcc-arm-embedded}/bin/arm-none-eabi-";
        buildPhase = "make";
        installPhase = "mkdir -p $out/bin";
      };
  };
}
