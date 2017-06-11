{ mkDerivation, attoparsec, base, containers, nix-derivation
, optparse-applicative, pretty-show, prettyprinter
, prettyprinter-ansi-terminal, protolude, stdenv, system-filepath
, text, vector
}:
mkDerivation {
  pname = "nix-derivation-pretty";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    attoparsec base containers nix-derivation optparse-applicative
    pretty-show prettyprinter prettyprinter-ansi-terminal protolude
    system-filepath text vector
  ];
  executableHaskellDepends = [ base ];
  homepage = "https://github.com/pbogdan/nix-derivation-pretty";
  description = "Pretty printer for Nix derivations";
  license = stdenv.lib.licenses.bsd3;
}
