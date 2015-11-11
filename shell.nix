{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, hspec, stdenv, text, yesod-core }:
      mkDerivation {
        pname = "yesod-csp";
        version = "0.1.0.0";
        src = ./.;
        libraryHaskellDepends = [ base text yesod-core ];
        testHaskellDepends = [ base hspec ];
        license = stdenv.lib.licenses.mit;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
