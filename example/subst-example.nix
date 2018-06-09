{ mkDerivation, base, subst, stdenv }:
mkDerivation {
  pname = "subst-example";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base subst ];
  license = stdenv.lib.licenses.bsd3;
}
