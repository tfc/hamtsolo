let
  nixpkgs = import ./nix/nixpkgs.nix;
  pkgs = import nixpkgs (import (builtins.fetchTarball https://github.com/input-output-hk/haskell.nix/archive/master.tar.gz));

  pkgSet = pkgs.haskell-nix.mkStackPkgSet {
    stack-pkgs = import ./pkgs.nix;
    modules = [];
  };
in
  pkgSet.config.hsPkgs
