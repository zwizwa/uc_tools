# Copied from Tom setup /i/exo/nix/nixpkgs-versions.nix
let
  v24_05 = builtins.fetchTarball {
    url = "https://releases.nixos.org/nixos/24.05/nixos-24.05.6668.e8c38b73aeb2/nixexprs.tar.xz";
    sha256 = "0lhh36z3fvd3b64dz7an08y3c3shb67aj17ny9z28bs21i3dc5yh";
  };
in v24_05

