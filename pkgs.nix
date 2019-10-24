{
  extras = hackage:
    { packages = { hamtsolo = ./hamtsolo.nix; }; };
  resolver = "lts-14.11";
  modules = [ ({ lib, ... }: { packages = {}; }) { packages = {}; } ];
  }