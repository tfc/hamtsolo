{
  nixpkgs ? <nixpkgs>,
  pkgs ? import nixpkgs {}
}:

let
  stackage = pkgs: import ./stack_overlay.nix { inherit pkgs; };
  pkgsMusl = pkgs.pkgsMusl;
in {
  inherit (stackage pkgs) hamtsolo;
  hamtsolo-static = (stackage pkgs.pkgsMusl).hamtsolo.overrideAttrs (old: {
    configureFlags = old.configureFlags ++ [
      "--ghc-option=-optl=-static"
      "--ghc-option=-optl=-pthread"
      "--extra-lib-dirs=${pkgsMusl.gmp6.override { withStatic = true; }}/lib"
      "--extra-lib-dirs=${pkgsMusl.zlib.static}/lib"
    ];
    installPhase = old.installPhase + ''
      mkdir -p $out/nix-support
      echo "file binary-dist $out/bin/hamtsolo" > $out/nix-support/hydra-build-products
    '';
  });
}
