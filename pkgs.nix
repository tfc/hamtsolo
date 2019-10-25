{
  extras = hackage:
    { packages = { hamtsolo = ./hamtsolo.nix; }; };
  resolver = "lts-13.19";
  modules = [ { packages = {}; } ];
  }