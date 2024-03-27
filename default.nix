{ compiler ? "ghc96", enableProfiling ? false, haskell, haskellPackages
, nix-gitignore, removeReferencesTo, symlinkJoin, makeWrapper }:
let
  src =
    nix-gitignore.gitignoreSource [ "default.nix" "flake.nix" "shell.nix" ] ./.;
  haskellPackages = haskell.packages.${compiler}.override {
    overrides = self: super: {
      # haskeline =
      #   haskell.lib.dontCheck (super.haskeline);
      ap-normalize = haskell.lib.dontCheck (super.ap-normalize);
    };
  };
  pkg = (haskellPackages.callCabal2nix "specstrom" "${src}" { });
  specstrom = (if enableProfiling then
    haskell.lib.enableExecutableProfiling pkg
  else
    haskell.lib.justStaticExecutables pkg);
  specstrom-wrapped = symlinkJoin {
    name = "specstrom";
    paths = [ specstrom ];
    buildInputs = [ makeWrapper ];
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
