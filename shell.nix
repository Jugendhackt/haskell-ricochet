{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { cabal-install, mkDerivation, base, lens, mtl, network, stdenv }:
      mkDerivation {
        pname = "haskell-ricochet";
        version = "0.1.0.0";
        src = ./.;
        libraryHaskellDepends = [ cabal-install base lens mtl network ];
        description = "ricochet reimplementation in Haskell";
        license = stdenv.lib.licenses.gpl3;
      };

  haskellPackages = if compiler == "default"
                      then pkgs.haskellPackages
                      else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
