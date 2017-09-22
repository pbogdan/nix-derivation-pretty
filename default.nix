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
    shell-cmd-src = (nixpkgs.fetchgit {
      url = "https://github.com/pbogdan/shell-cmd";
      rev = "d6c8435286edaa451a34afe1d5a7940a377e643f";
    });
in
nixpkgs
 .pkgs
 .haskell
 .packages
 .${compiler}
.callPackage ./nix-derivation-pretty.nix rec {
   shell-cmd = import (shell-cmd-src) { inherit nixpkgs compiler; };
}
