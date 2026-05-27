let
  nixpkgs = import ../nix/nixpkgs.nix;
in
with import nixpkgs { };

let dummy = 123;

in mkShell {
  buildInputs = [
    gcc
  ];
}


