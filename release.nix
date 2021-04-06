{
  niv ? import ./nix/sources.nix,
  nixpkgs ? niv.nixpkgs
}:

let
  haskell-nix = import niv.haskell-nix;
  pkgs = import nixpkgs { inherit (haskell-nix) overlays; };
  pkgSet = p: p.haskell-nix.stackProject {
    src = p.haskell-nix.haskellLib.cleanGit {
      name = "hamtsolo";
      src = ./.;
    };
  };
in {
  hamtsolo = (pkgSet pkgs).hamtsolo.components.all;
  hamtsolo-static = (pkgSet pkgs.pkgsMusl).hamtsolo.overrideAttrs (old: {
    configureFlags = old.configureFlags ++ [
      "--ghc-option=-optl=-static"
      "--ghc-option=-optl=-pthread"
      "--extra-lib-dirs=${pkgs.pkgsMusl.gmp6.override { withStatic = true; }}/lib"
      "--extra-lib-dirs=${pkgs.pkgsMusl.zlib.static}/lib"
    ];
    installPhase = old.installPhase + ''
      mkdir -p $out/nix-support
      echo "file binary-dist $out/bin/hamtsolo" > $out/nix-support/hydra-build-products
    '';
  }).hamtsolo.components.all;
}
