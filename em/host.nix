# Skeleton "C in browser" app speaking the uc_tools message protocol.
let
  nixpkgs = import ../nix/nixpkgs.nix;
in
with import nixpkgs { };

let abc = "def";

in mkShell {
  buildInputs = [
    emscripten
    xxd # for xxd -i : generate header file from data
  ];

  # Emscripten's default cache lives in the read-only nix store. Point it at a
  # writable dir so emcc can link. The copy seeds prebuilt sysroot libs; if the
  # path is wrong it falls back to rebuilding them (slower, still offline).
  #shellHook = ''
  #  export EM_CACHE=$(mktemp -d)
  #  cp -r ${emscripten}/share/emscripten/cache/* "$EM_CACHE"/ 2>/dev/null || true
  #'';

  # It is actually using:
  # /home/tom/.emscripten_cache/sysroot/lib/wasm32-emscripten
}


