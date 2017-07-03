{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc802" }:
nixpkgs
 .pkgs
 .haskell
 .packages
 .${compiler}
 .callPackage ./nix-derivation-pretty.nix rec { }
