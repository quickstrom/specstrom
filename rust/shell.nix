{ pkgs ? import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/98747f27ecfee70c8c97b195cbb94df80a074dda.tar.gz") {} }:

let
in pkgs.mkShell {
  buildInputs = [
    pkgs.bashInteractive
    pkgs.cargo
    pkgs.cargo-watch
    pkgs.rustc
    pkgs.rls
    pkgs.rustfmt
    pkgs.libiconv
  ];
}
