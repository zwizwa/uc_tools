{
  description = "libopencm3";
  # Unstable snapshot.
  inputs.nixpkgs.url = github:zwizwa/libopencm3/3eff201a4bb3759a9c967a6f5e3fd0aad46dc5af

  outputs = { self, nixpkgs }:
    let pkgs = nixpkgs.legacyPackages.x86_64-linux;
    in
  {
    packages.x86_64-linux.default =
      with import nixpkgs { system = "x86_64-linux"; };
      stdenv.mkDerivation {
        name = "libopencm3";
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
