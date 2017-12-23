{ pkgs ? null, compiler ? null }:
(import ./default.nix { inherit pkgs compiler; }).env
