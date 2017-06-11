{ mkDerivation, ansi-terminal, base, doctest, prettyprinter, stdenv
, text
}:
mkDerivation {
  pname = "prettyprinter-ansi-terminal";
  version = "0.1";
  sha256 = "0n42hapidn94pq0jw0854l42pwwp3bpy0b9x685anzh1lkf0djrp";
  revision = "1";
  editedCabalFile = "4ae4afd13fddfe4a05192c44c711eb417177288aa67728e040af57998e4fcb63";
  libraryHaskellDepends = [ ansi-terminal base prettyprinter text ];
  testHaskellDepends = [ base doctest ];
  homepage = "http://github.com/quchen/prettyprinter";
  description = "ANSI terminal backend for the modern, extensible and well-documented prettyprinter";
  license = stdenv.lib.licenses.bsd2;
}
