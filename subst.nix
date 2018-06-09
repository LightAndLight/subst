{ mkDerivation, base, lens, rev-state, stdenv }:
mkDerivation {
  pname = "subst";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base lens rev-state ];
  license = stdenv.lib.licenses.bsd3;
}
