{ pkgs ? import <nixpkgs> {}, compiler ? "ghc884", enableProfiling ? false }:
let
  src = pkgs.nix-gitignore.gitignoreSource [ ] ./.;
  haskellPackages = pkgs.haskell.packages.${compiler};
  drv = (haskellPackages.callCabal2nix "specstrom" "${src}" { });
in {
  inherit haskellPackages;
  specstrom = 
    if enableProfiling 
    then pkgs.haskell.lib.enableExecutableProfiling drv
    else pkgs.haskell.lib.justStaticExecutables drv;
}
