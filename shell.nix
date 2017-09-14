{ pkgs ? null, compiler ? "ghc802" }:
(import ./default.nix { inherit pkgs compiler; }).env
