{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { cabal-install, mkDerivation, attoparsec, base, base32-bytestring
      , base32string , base64-bytestring, bytestring , containers, hprotoc
      , HsOpenSSL, lens , mtl, network , network-anonymous-tor, socks, stdenv
      , transformers
      }:
      mkDerivation {
        pname = "haskell-ricochet";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [ cabal-install
          attoparsec base base32-bytestring base32string base64-bytestring
          bytestring containers hprotoc HsOpenSSL lens mtl network
          network-anonymous-tor socks transformers
        ];
        executableHaskellDepends = [
          base bytestring containers lens mtl network
        ];
        description = "ricochet reimplementation in Haskell";
        license = stdenv.lib.licenses.gpl3;
      };


  haskellPackages = if compiler == "default"
                      then pkgs.haskellPackages
                      else pkgs.haskell.packages.${compiler};

  over = h: { name, file }: h.override (x: {overrides = (self: super:
        {"${name}" = h.callPackage (import file) {};});});

  entry = name : { inherit name; file = ./. + "/${name}.nix"; };

  haskellPackages' = builtins.foldl' over haskellPackages [
                      (entry "protocol-buffers")
                      (entry "protocol-buffers-descriptor")
                      (entry "hprotoc")
                      (entry "network-anonymous-tor")
                    ];

  drv = haskellPackages'.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
