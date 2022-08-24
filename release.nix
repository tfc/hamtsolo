{ niv ? import ./nix/sources.nix
, nixpkgs ? niv.nixpkgs
, pkgs ? import nixpkgs { }
}:

let
  src = pkgs.lib.sourceByRegex ./. [
    "src"
    "src/.*\.hs$"
    "hamtsolo.cabal"
    "Setup.hs"
    "LICENSE"
  ];
in
{
  hamtsolo = pkgs.haskellPackages.callCabal2nix "hamtsolo" src { };
  hamtsolo-static =
    let
      pkgsStatic = pkgs.pkgsMusl;
      inherit (pkgsStatic.haskell.lib) appendConfigureFlags justStaticExecutables;
      hamtsoloMusl = pkgsStatic.haskellPackages.callCabal2nix "hamtsolo" src { };
      staticLibs = [
        (pkgsStatic.gmp6.override { withStatic = true; })
        (pkgsStatic.libffi.overrideAttrs (old: { dontDisableStatic = true; }))
        (pkgsStatic.ncurses.override { enableStatic = true; })
        pkgsStatic.zlib.static
      ];
      staticFlags = [
        "--disable-shared"
        "--ghc-option=-optl=-static"
      ] ++ builtins.map (l: "--extra-lib-dirs=${l}/lib") staticLibs;
    in
    appendConfigureFlags (justStaticExecutables hamtsoloMusl) staticFlags;
}
