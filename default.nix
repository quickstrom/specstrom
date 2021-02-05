{ pkgs ? import <nixpkgs> {}, compiler ? "ghc884" }:
let
  src = pkgs.nix-gitignore.gitignoreSource [ ] ./.;
  haskellPackages = pkgs.haskell.packages.${compiler};
in {
  inherit haskellPackages;
  specstrom = haskellPackages.callCabal2nix "specstrom" "${src}" { };
}
