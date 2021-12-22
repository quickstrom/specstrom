{ pkgs ? import ./nix/nixpkgs.nix { }, compiler ? "ghc8104", enableProfiling ? false }:
let
  src = pkgs.nix-gitignore.gitignoreSource [ "default.nix" ] ./.;
  haskellPackages = pkgs.haskell.packages.${compiler}.override {
    overrides = self: super: {
      haskeline =
        pkgs.haskell.lib.dontCheck (self.callHackage "haskeline" "0.8.0.0" { });
    };
  };
  pkg = (haskellPackages.callCabal2nix "specstrom" "${src}" { });
  specstrom = (if enableProfiling then
    pkgs.haskell.lib.enableExecutableProfiling pkg
  else
    pkgs.haskell.lib.justStaticExecutables pkg).overrideAttrs(drv: {
      # Various hacks to get the closure size down, similar to how Pandoc is set up in nixpkgs:
      # https://github.com/NixOS/nixpkgs/blob/40662d31b8e6f4abdb813d4d9c33d71f1b7a922c/pkgs/development/tools/pandoc/default.nix
      disallowedReferences = [haskellPackages.pandoc haskellPackages.pandoc-types haskellPackages.HTTP];
      postInstall = ''
        ${pkgs.removeReferencesTo}/bin/remove-references-to -t ${haskellPackages.pandoc} $out/bin/docstrom
        ${pkgs.removeReferencesTo}/bin/remove-references-to -t ${haskellPackages.pandoc-types} $out/bin/docstrom
        ${pkgs.removeReferencesTo}/bin/remove-references-to -t ${haskellPackages.HTTP} $out/bin/docstrom
      '';
    });
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
