{
  description = "Quickstrom: an autonomous testing tool for the web";

  inputs.nixpkgs.url =
    "github:NixOS/nixpkgs/ede02b4ccb13557b95058d66146640a2b0bb198f";
  inputs.systems.url = "github:nix-systems/default";

  outputs = { self, systems, nixpkgs }:
    let eachSystem = nixpkgs.lib.genAttrs (import systems);
    in {
      packages = eachSystem (system:
        let
          pkgs = (import nixpkgs { inherit system; });
          specstrom = import ./. { inherit pkgs; };
        in { default = specstrom.specstrom; });
      devShells = eachSystem (system:
        let
          pkgs = (import nixpkgs { inherit system; });
          specstrom = import ./. { inherit pkgs; };
        in { default = specstrom.package; });
    };
}
