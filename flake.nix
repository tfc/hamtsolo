{
  description = "Hamtsolo AMT Serial-Over-Lan Client";

  inputs = {
    flake-parts.url = "github:hercules-ci/flake-parts";
    flake-parts.inputs.nixpkgs-lib.follows = "nixpkgs";
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
  };

  outputs = inputs: inputs.flake-parts.lib.mkFlake { inherit inputs; } {
    systems = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];
    perSystem = { config, pkgs, system, ... }:
      let
        src = pkgs.lib.fileset.toSource {
          root = ./.;
          fileset = pkgs.lib.fileset.unions [
            ./src
            ./hamtsolo.cabal
            ./Setup.hs
            ./LICENSE
          ];
        };
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
