{ pkgs ? import <nixpkgs> {}, compiler ? "ghc8102" }:
let
  project = import ./. { inherit pkgs; };
in project.haskellPackages.shellFor {
  withHoogle = true;
  packages = (p: [project.specstrom]);
  buildInputs = (with pkgs; [
    cabal-install
    nixfmt
    project.haskellPackages.haskell-language-server
    project.haskellPackages.ormolu
  ]);
}
