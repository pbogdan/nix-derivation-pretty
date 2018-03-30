{ pkgs ? null, compiler ? null}:
let nixpkgs = if pkgs == null then
              import ((import <nixpkgs> {}).fetchFromGitHub {
                owner = "NixOS";
                repo = "nixpkgs";
                rev = "b6ddb9913f2";
                sha256 = "1yjbd5jhjgirv7ki0sms1x13dqyjzg0rpb7n67m6ik61fmpq0nfw";
             }) {}
             else
             import <nixpkgs> {};
    hsPkgSet = if compiler == null then
               nixpkgs.haskellPackages
               else
               nixpkgs.haskell.packages.${compiler};
in hsPkgSet.callPackage ./nix-derivation-pretty.nix rec { }
