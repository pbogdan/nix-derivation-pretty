{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc802" }:
let nix-deriviation-src = (nixpkgs.fetchgit {
      url = "https://github.com/Gabriel439/Haskell-Nix-Derivation-Library";
      rev = "c178a6defa5cb55fd9d01794a9efc7dc56bbf1a6";
    });
in nixpkgs
   .pkgs
   .haskell
   .packages
   .${compiler}
   .callPackage ./nix-derivation-pretty.nix rec {
     nix-derivation = nixpkgs
                      .pkgs
                      .haskell
                      .packages
                      .${compiler}
                      .callPackage (nix-deriviation-src) { };
     prettyprinter = nixpkgs
                     .pkgs
                     .haskell
                     .packages
                     .${compiler}
                     .callPackage ./deps/prettyprinter.nix { };
     prettyprinter-ansi-terminal = nixpkgs
                     .pkgs
                     .haskell
                     .packages
                     .${compiler}
                     .callPackage ./deps/prettyprinter-ansi-terminal.nix { inherit prettyprinter; };
}
