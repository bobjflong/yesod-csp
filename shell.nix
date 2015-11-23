{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, attoparsec, base, hspec, mono-traversable
      , network-uri, semigroups, stdenv, syb, template-haskell, text
      , uniplate, yesod, yesod-core, yesod-test
      }:
      mkDerivation {
        pname = "yesod-csp";
        version = "0.1.1.0";
        src = ./.;
        libraryHaskellDepends = [
          attoparsec base mono-traversable network-uri semigroups syb
          template-haskell text uniplate yesod yesod-core
        ];
        testHaskellDepends = [
          attoparsec base hspec network-uri semigroups template-haskell yesod
          yesod-test
        ];
        description = "Add CSP headers to Yesod apps";
        license = stdenv.lib.licenses.mit;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
