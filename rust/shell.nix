{ pkgs ? import <nixpkgs> { } }:
let
in pkgs.mkShell {
  buildInputs =
    [ pkgs.bashInteractive pkgs.cargo pkgs.rustc pkgs.rls pkgs.libiconv ];
}
