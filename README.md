# My Dotfiles as a Bare Git Repository

This repository contains all my linux configuration files I use for my personal setup, including for example Doom Emacs and Z Shell.

I created this repository according to [this guide](https://www.atlassian.com/git/tutorials/dotfiles) from Atlassian.

## Notes

- The NixOS configuration lives in `nix/` and is applied with `pb-nixos-rebuild-switch` (see `.zshrc`).
- `nix/configuration.nix` imports `nix/hardware-configuration.nix`, which is intentionally not part of this repository because it is specific to each machine.
  Generate it on a new machine with `nixos-generate-config --show-hardware-config > ~/nix/hardware-configuration.nix`.
