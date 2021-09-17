{ pkgs ? import ./nix/nixpkgs.nix { }, compiler ? "ghc8104", enableProfiling ? false }:
let
  src = pkgs.nix-gitignore.gitignoreSource [ ] ./.;
  haskellPackages = pkgs.haskell.packages.${compiler}.override {
    overrides = self: super: {
      haskeline =
        pkgs.haskell.lib.dontCheck (self.callHackage "haskeline" "0.8.0.0" { });
    };
  };
  pkg = (haskellPackages.callCabal2nix "specstrom" "${src}" { });
  specstrom = if enableProfiling then
    pkgs.haskell.lib.enableExecutableProfiling pkg
  else
    pkgs.haskell.lib.justStaticExecutables pkg;
  specstrom-wrapped = pkgs.symlinkJoin {
    name = "specstrom";
    paths = [ specstrom ];
    buildInputs = [ pkgs.makeWrapper ];
    postBuild = ''
      mkdir -p $out/share
      cp -r ${./ulib} $out/share/ulib
      wrapProgram $out/bin/specstrom \
        --add-flags "-I$out/share/ulib"
    '';
  };
in {
  inherit haskellPackages;
  package = pkg;
  specstrom = specstrom-wrapped;
}
