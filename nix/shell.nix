{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { cabal-install, mkDerivation, attoparsec, base, bytestring, containers
      , hprotoc, lens, mtl, network, network-anonymous-tor, socks, stdenv
      , transformers
      }:
      mkDerivation {
        pname = "haskell-ricochet";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [ cabal-install
          attoparsec base bytestring containers hprotoc lens mtl network
          network-anonymous-tor socks transformers
        ];
        executableHaskellDepends = [
          base bytestring containers lens mtl network
        ];
        description = "ricochet reimplementation in Haskell";
        license = stdenv.lib.licenses.gpl3;
      };

  over = h: n: f: h.override (x: {overrides = (self: super: {"${n}" =
  h.callPackage (import f) {};});});


  haskellPackages = if compiler == "default"
                      then pkgs.haskellPackages
                      else pkgs.haskell.packages.${compiler};

  haskellPackages2 = over haskellPackages "protocol-buffers" ./protocol-buffers.nix;
  haskellPackages3 = over haskellPackages2 "protocol-buffers-descriptor" ./protocol-buffers-descriptor.nix;
  haskellPackages4 = over haskellPackages3 "hprotoc" ./hprotoc.nix;

  drv = haskellPackages4.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
