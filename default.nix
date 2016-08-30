let nixpkgs = import <nixpkgs> {};
    old     = import (nixpkgs.fetchFromGitHub {
      owner = "NixOS";
      repo = "nixpkgs";
      rev = "39b48ed2effa2bf2c4241869ec78d5bcbaac5a3d";
      sha256 = "1592sszn3f98v3pllbfcy5ad558ynj447pqm4hvb5kj766nfm69k";
    }) {};
  in old.haskellPackages.callPackage ./haskell-ricochet.nix {}
