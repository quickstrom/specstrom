{
  description = "Specstrom: an autonomous testing tool for the web";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.11";
  inputs.systems.url = "github:nix-systems/default";

  outputs = { self, systems, nixpkgs }:
    let eachSystem = nixpkgs.lib.genAttrs (import systems);
    in {
      packages = eachSystem (system:
        let
          pkgs = (import nixpkgs { inherit system; });
          specstrom = pkgs.callPackage ./. { };
        in { default = specstrom.specstrom; });
      devShells = eachSystem (system:
        let pkgs = (import nixpkgs { inherit system; });
        in { default = pkgs.callPackage ./shell.nix { }; });
    };
}
