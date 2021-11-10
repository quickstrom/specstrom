{ pkgs ? import ./nix/nixpkgs.nix {}, compiler ? "ghc8104" }:
let
  project = import ./. { inherit pkgs compiler; };
in project.haskellPackages.shellFor {
  withHoogle = true;
  packages = (p: [project.package]);
  buildInputs = (with pkgs; [
    cabal-install
    nixfmt
    project.haskellPackages.haskell-language-server
    ormolu
    ghcid
  ]);
}
