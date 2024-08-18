{ compiler ? "ghc96", callPackage, cabal-install, nixfmt, ormolu, ghcid }:
let project = callPackage ./. { inherit compiler; };
in project.haskellPackages.shellFor {
  withHoogle = false;
  packages = (p: [ project.package ]);
  nativeBuildInputs = [
    project.haskellPackages.cabal-install
    project.haskellPackages.haskell-language-server
    # nixfmt
    # ormolu
    # ghcid
  ];
}
