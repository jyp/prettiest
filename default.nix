{ mkDerivation, base, containers, stdenv }:
mkDerivation {
  pname = "pretty-compact";
  version = "2.0";
  src = ./.;
  libraryHaskellDepends = [ base containers ];
  description = "Pretty-printing library";
  license = "GPL";
}
