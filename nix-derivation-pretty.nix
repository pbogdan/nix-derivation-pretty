{ mkDerivation, attoparsec, base, bifunctors, containers, extra
, nix-derivation, optparse-applicative, pretty-show, prettyprinter
, prettyprinter-ansi-terminal, protolude, shell-cmd, stdenv
, system-filepath, temporary, text, vector
}:
mkDerivation {
  pname = "nix-derivation-pretty";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    attoparsec base bifunctors containers extra nix-derivation
    optparse-applicative pretty-show prettyprinter
    prettyprinter-ansi-terminal protolude shell-cmd system-filepath
    temporary text vector
  ];
  executableHaskellDepends = [ base ];
  homepage = "https://github.com/pbogdan/nix-derivation-pretty";
  description = "Pretty printer for Nix derivations";
  license = stdenv.lib.licenses.bsd3;
}
