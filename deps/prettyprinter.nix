{ mkDerivation, ansi-wl-pprint, base, bytestring, criterion
, doctest, mtl, pgp-wordlist, QuickCheck, random, stdenv, tasty
, tasty-quickcheck, template-haskell, text, transformers
}:
mkDerivation {
  pname = "prettyprinter";
  version = "0.1";
  sha256 = "0n45ag89xlcs3sfpwhghfqgwradldlzs8rgkn5z6747s7v2m40aj";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base text ];
  executableHaskellDepends = [ base template-haskell text ];
  testHaskellDepends = [
    base bytestring doctest pgp-wordlist QuickCheck tasty
    tasty-quickcheck text
  ];
  benchmarkHaskellDepends = [
    ansi-wl-pprint base criterion mtl random text transformers
  ];
  homepage = "http://github.com/quchen/prettyprinter";
  description = "A modern, extensible and well-documented prettyprinter";
  license = stdenv.lib.licenses.bsd2;
}
