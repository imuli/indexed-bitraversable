{ pkgs ? import <nixpkgs> {}
, hpkgs ? pkgs.haskellPackages
, mkDerivation ? expr: hpkgs.mkDerivation (expr // { doHaddock = true; })
}:
hpkgs.callCabal2nix "bases" (pkgs.lib.cleanSource ./.) { inherit mkDerivation; }
