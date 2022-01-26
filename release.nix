{ niv ? import ./nix/sources.nix
, nixpkgs ? niv.nixpkgs
, pkgs ? import nixpkgs { }
}:

let
  sourceByRegex = src: regexes:
    builtins.filterSource
      (
        path: type:
          let
            relPath = pkgs.lib.removePrefix "${toString src}/" (toString path);
            match = builtins.match (pkgs.lib.strings.concatStringsSep "|" regexes);
          in
          (type == "directory" && match (relPath + "/") != null || match relPath != null)
      )
      src;

  src = sourceByRegex ./. [
    "src/"
    "src/.*\.hs"
    ".*\.cabal"
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
    in
    appendConfigureFlags (justStaticExecutables hamtsoloMusl)
      [
        "--disable-shared"
        "--ghc-option=-optl=-static"
        "--extra-lib-dirs=${pkgsStatic.gmp6.override { withStatic = true; }}/lib"
        "--extra-lib-dirs=${pkgsStatic.zlib.static}/lib"
        "--extra-lib-dirs=${pkgsStatic.libffi.overrideAttrs (old: { dontDisableStatic = true; })}/lib"
        "--extra-lib-dirs=${pkgsStatic.ncurses.override { enableStatic = true; }}/lib"
      ];
}
