{
  description = "Hamtsolo AMT Serial-Over-Lan Client";

  inputs = {
    flake-parts.url = "github:hercules-ci/flake-parts";
    nixpkgs.url = "github:nixos/nixpkgs";
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
  };

  outputs = inputs: inputs.flake-parts.lib.mkFlake { inherit inputs; } {
    systems = [ "x86_64-linux" ];
    perSystem = { config, pkgs, system, ... }:
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
        packages = {
          default = config.packages.hamtsolo;
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
        };

        devShells.default = pkgs.haskellPackages.shellFor {
          packages = p: [ config.packages.hamtsolo ];
          buildInputs = with pkgs; [
            cabal-install
            ghcid
          ];
          shellHook = ''
            ${config.checks.pre-commit-check.shellHook}
          '';
        };

        checks = {
          pre-commit-check = inputs.pre-commit-hooks.lib.${system}.run {
            src = ./.;
            hooks = {
              cabal-fmt.enable = true;
              hlint.enable = true;
              nixpkgs-fmt.enable = true;
              statix.enable = true;
            };
          };
        };
      };
  };
}
