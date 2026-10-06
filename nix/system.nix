# Entry point for nixos-rebuild (NixOS 26.05 and later).
#
# Builds the system from the nixpkgs pinned with lon (./lon.lock), instead of
# a channel or <nixpkgs>. nixos-rebuild finds this file through
# /etc/nixos/system.nix (see configuration.nix), so a plain
# `sudo nixos-rebuild switch` uses the pin. pb-nixos-rebuild-switch passes
# it explicitly with --file.
let
  sources = import ./lon.nix;
in
import "${sources.nixpkgs}/nixos" {
  configuration = ./configuration.nix;
}
