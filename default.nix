let
  nixpkgs = import ./nix/nixpkgs.nix;
  pkgs = import nixpkgs (import (builtins.fetchTarball https://github.com/input-output-hk/haskell.nix/archive/master.tar.gz));

  pkgSet = pkgs.haskell-nix.stackProject {
    src = pkgs.haskell-nix.haskellLib.cleanGit { src = ./.; };
  };
in
  pkgSet.hamtsolo.components.all
