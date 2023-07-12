with import <nixpkgs> {};
mkShell {
  buildInputs = [
    gcc racket
    busybox # unix2dos

    # keep this line if you use bash
    bashInteractive
  ];
}
