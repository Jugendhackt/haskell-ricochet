{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { cabal-install, mkDerivation, base, bytestring, lens, mtl, network
      , network-anonymous-tor, stdenv, transformers
      }:
      mkDerivation {
        pname = "haskell-ricochet";
        version = "0.1.0.0";
        src = ./.;
        libraryHaskellDepends = [ cabal-install
          base bytestring lens mtl network network-anonymous-tor transformers
        ];
        description = "ricochet reimplementation in Haskell";
        license = stdenv.lib.licenses.gpl3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
