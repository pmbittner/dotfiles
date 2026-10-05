# My Dotfiles as a Bare Git Repository

This repository contains all my linux configuration files I use for my personal setup, including for example Doom Emacs and Z Shell.

I created this repository according to [this guide](https://www.atlassian.com/git/tutorials/dotfiles) from Atlassian.

## Notes

- The NixOS configuration lives in `nix/` and is applied with `pb-nixos-rebuild-switch` (see `.zshrc`).
- `nix/configuration.nix` imports `nix/hardware-configuration.nix`, which is intentionally not part of this repository because it is specific to each machine.
  Generate it on a new machine with `nixos-generate-config --show-hardware-config > ~/nix/hardware-configuration.nix`.
- Styling and some programs are managed with [home-manager](https://github.com/nix-community/home-manager) and [stylix](https://github.com/nix-community/stylix), both pinned with lon like nixpkgs itself (`pb-nixos-update-pins` updates the pins). This only applies to programs that are used on the NixOS machine alone; everything else stays a plain, portable dotfile. See [docs/STYLE.md](docs/STYLE.md) for which programs are managed and how the styling works.
