let
  niv = import ./nix/sources.nix;
  inherit (niv) nixpkgs haskell-nix;
  overlays = (import haskell-nix).overlays;
  pkgs = import nixpkgs { inherit overlays; };

  pkgSet = pkgs.haskell-nix.stackProject {
    src = pkgs.haskell-nix.haskellLib.cleanGit {
      name = "hamtsolo";
      src = ./.;
    };
  };
in
  pkgSet.hamtsolo.components.all
