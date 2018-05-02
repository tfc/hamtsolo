{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false, buildStatic ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, async, attoparsec, attoparsec-binary, base
      , binary, bytestring, conduit, conduit-combinators, conduit-extra
      , exceptions, gitrev, optparse-applicative, resourcet, stdenv
      , stm-conduit, unix
      }:
      mkDerivation {
        pname = "hamtsolo";
        version = "1.0.2";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [
          async attoparsec attoparsec-binary base binary bytestring conduit
          conduit-combinators conduit-extra exceptions gitrev
          optparse-applicative resourcet stm-conduit unix
        ];

        enableSharedExecutables = !buildStatic;
        enableSharedLibraries = !buildStatic;
        configureFlags = if buildStatic then [
          "--ghc-option=-optl=-static"
          "--ghc-option=-optl=-L${pkgs.gmp6.override { withStatic = true; }}/lib"
          "--ghc-option=-optl=-L${pkgs.zlib.static}/lib"
          "--ghc-option=-optl=-L${pkgs.glibc.static}/lib"
        ] else [];

        homepage = "https://github.com/tfc/hamtsolo#readme";
        description = "Intel AMT serial-over-lan (SOL) client";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
