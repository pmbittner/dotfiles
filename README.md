# My Dotfiles as a Bare Git Repository

This repository contains all my linux configuration files I use for my personal setup, including for example Doom Emacs and Z Shell.

I created this repository according to [this guide](https://www.atlassian.com/git/tutorials/dotfiles) from Atlassian.

## Notes

- The NixOS configuration lives in `nix/`, see [docs/NIX.md](docs/NIX.md). `nix/hardware-configuration.nix` is machine-specific and not part of this repository; NIX.md explains how to generate it.
- Styling and some programs are managed with [home-manager](https://github.com/nix-community/home-manager) and [stylix](https://github.com/nix-community/stylix), both pinned with lon like nixpkgs itself. This only applies to programs that are used on the NixOS machine alone; everything else stays a plain, portable dotfile. See [docs/STYLE.md](docs/STYLE.md) for which programs are managed and how the styling works.
