{ pkgs ? import <nixpkgs> {}, hpkgs ? pkgs.haskellPackages, withHoogle ? true }:
let drv = hpkgs.callPackage ./. { inherit pkgs; };
 in hpkgs.shellFor {
      packages = p: [ drv ];
      inherit withHoogle;
    }
