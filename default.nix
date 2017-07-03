{ pkgs ? null, compiler ? "ghc802" }:
let nixpkgs = if pkgs == null then
              import ((import <nixpkgs> {}).fetchFromGitHub {
                owner = "NixOS";
                repo = "nixpkgs";
                rev = "2e983f14f6";
                sha256 = "0rm5kb8l27kb2bac95zqc18v50065iix83sv3l0v9wpkq7421cby";
             }) {}
             else
             import <nixpkgs> {};
in
nixpkgs
 .pkgs
 .haskell
 .packages
 .${compiler}
 .callPackage ./nix-derivation-pretty.nix rec { }
