# My Dotfiles as a Bare Git Repository

This repository contains all my linux configuration files I use for my personal setup, including for example Doom Emacs and Z Shell.

I created this repository according to [this guide](https://www.atlassian.com/git/tutorials/dotfiles) from Atlassian.

## Notes

- The NixOS configuration lives in `nix/` and is applied with `pb-nixos-rebuild-switch` (see `.zshrc`).
- `nix/configuration.nix` imports `nix/hardware-configuration.nix`, which is intentionally not part of this repository because it is specific to each machine.
  Generate it on a new machine with `nixos-generate-config --show-hardware-config > ~/nix/hardware-configuration.nix`.
- Styling and some programs are managed with [home-manager](https://github.com/nix-community/home-manager) and [stylix](https://github.com/nix-community/stylix), both pinned with lon (`pb-nixos-update-pins` updates the pins). This only applies to programs that are used on the NixOS machine alone. Everything else stays a plain, portable dotfile.
  - Managed by home-manager (config in `nix/modules/home/`, listed in `nix/modules/home.nix`): rofi, dunst, waybar, wlogout, and the GTK theme (thunar and other GTK apps). Their colors and fonts come from stylix in `nix/modules/style.nix`.
  - Not managed by home-manager: Doom Emacs, ranger, nvim, zsh, hyprland (`.config/hypr/hyprland.lua`) and the other dotfiles in this repository. Kitty is a special case: its `kitty.conf` stays a plain dotfile, home-manager only generates a colors file for it (`nix/modules/home/kitty.nix`), which `kitty.conf` can include (currently disabled).
