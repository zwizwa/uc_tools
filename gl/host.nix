let
  nixpkgs = import ../nix/nixpkgs.nix;
in
with import nixpkgs { };

let abc = "def";

in mkShell {
  nativeBuildInputs = [
    pkg-config
  ];
  buildInputs = [
    xxd
    SDL2
  ];
}


