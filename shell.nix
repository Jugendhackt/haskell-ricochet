{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, mtl, network, stdenv }:
      mkDerivation {
        pname = "haskell-ricochet";
        version = "0.1.0.0";
        src = ./.;
        libraryHaskellDepends = [ base mtl network ];
        description = "ricochet reimplementation in Haskell";
        license = stdenv.lib.licenses.gpl3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
