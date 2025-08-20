{ mkDerivation, base, lib, Stream }:
mkDerivation {
  pname = "uc-tools";
  version = "1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base ];
  license = lib.licenses.bsd3;
}
