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
    pkg = hsPkgSet.callPackage ./nix-derivation-pretty.nix rec { };
in
    with nixpkgs.haskell.lib;
    overrideCabal pkg (drv: {
      enableSharedExecutables = false;
      postInstall = ''
        exe=$out/bin/pp-drv
        mkdir -p $out/share/{bash-completion/completions,zsh/vendor-completions,fish/completions}
        $exe --bash-completion-script $exe >$out/share/bash-completion/completions/pp-drv
        $exe --zsh-completion-script $exe >$out/share/zsh/vendor-completions/_pp-drv
        $exe --fish-completion-script $exe >$out/share/fish/completions/pp-drv.fish
      '';
    })
