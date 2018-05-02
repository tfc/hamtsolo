let
  hamtsolo = import ./default.nix;
in
  {
    hamtsolo_dynamic = hamtsolo { };
    hamtsolo_static = hamtsolo { buildStatic = true; };
  }
