{ pkgs ? import <nixpkgs> {}, compiler ? "ghc884", enableProfiling ? false }:
let
  src = pkgs.nix-gitignore.gitignoreSource [ ] ./.;
  haskellPackages = pkgs.haskell.packages.${compiler}.override {
    overrides = self: super: {
      haskeline = pkgs.haskell.lib.dontCheck (self.callHackage "haskeline" "0.8.0.0" {});
    };
  };
  drv = (haskellPackages.callCabal2nix "specstrom" "${src}" { });
in {
  inherit haskellPackages;
  specstrom = 
    if enableProfiling 
    then pkgs.haskell.lib.enableExecutableProfiling drv
    else pkgs.haskell.lib.justStaticExecutables drv;
}
