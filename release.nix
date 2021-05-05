{
  niv ? import ./nix/sources.nix,
  nixpkgs ? niv.nixpkgs,
  pkgs ? import nixpkgs {}
}:

{
  hamtsolo = pkgs.haskellPackages.callCabal2nix "hamtsolo" ./. {};
}
