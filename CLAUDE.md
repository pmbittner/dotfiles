# Dotfiles

Personal dotfiles for a NixOS desktop (host `perry`) with Hyprland. Navigation and usage follow vim keybindings wherever possible. The look should be simple but modern. The main editor is Doom Emacs.

## Design Philosophy

- Any persistent settings or setup should stay as text files.
- System configuration is declarative and reproducible.
- Developing the dotfiles should be a learning experience.
- Doom Emacs and the terminal are the main ways to interact with the system.

## Documentation

Read the matching document before working on a topic:

- `docs/NIX.md`: NixOS structure, pinned sources (lon), rebuilding, updating and upgrading, machine-specific values.
- `docs/DESKTOP.md`: Hyprland, keybindings and the desktop tools, and why they were chosen.
- `docs/STYLE.md`: styling (stylix, home-manager), what is themed and how, pitfalls.
- `docs/PLANS.md`: open goals, todos and decisions. Pick the next task from there; remove a goal when it is done.

## Repository setup

- This is a bare git repository. On the Linux machine, the work tree is `$HOME` and the git directory is `~/.myconfig.git`, used through the `config` function in `.zshrc` (`config status`, `config add`, ...). Paths in this repo are paths relative to `$HOME`.
- The repo is usually edited from a normal clone on macOS. NixOS and Hyprland cannot be built or run there, so changes are tested on the Linux machine.
- A few files are for macOS only (`.config/aerospace/`, `zsh/mac.zsh`); `.zshrc` is shared by both systems.
- `.config/nvim` is a git submodule.

## Layout

- `nix/`: the NixOS configuration (`docs/NIX.md`). Home-manager and stylix are used only for programs that exist on perry alone (`docs/STYLE.md`); everything else stays a plain, portable dotfile.
- `.config/hypr/hyprland.lua`: Hyprland config, see `docs/DESKTOP.md`.
- `.config/networkmanager-dmenu/config.ini`: config of the rofi Wi-Fi menu.
- `bin/`: helper scripts called from Hyprland (`~/bin`).
- `.zshrc` and `zsh/`: `.zshrc` sets up oh-my-zsh and p10k and sources one file per topic from `zsh/` (`git`, `nix`, `emacs`, `files`, `media`, `system`, `apps`, `dev`, and `mac.zsh` on macOS). New functions and aliases go into the matching topic file; machine-specific ones into the untracked `~/.local.zsh`.

## Commands

- Apply the NixOS config (on Linux): `pb-nixos-rebuild-switch`. More commands in `docs/NIX.md`.
- Check the Hyprland config (on Linux): `make test` in `.config/hypr`.
- Syntax checks that work anywhere: `nix-instantiate --parse <file>.nix`, `shellcheck bin/*.sh`, `nixfmt` for formatting.

## Conventions

- Commit messages: `<area>: <short lowercase summary>`, e.g. `nix: ...`, `hypr: ...`, `zsh: ...`, `bin: ...`. One logical change per commit. Optionally, add sub-area in brackets like `<area>(<subarea>): ...` such as `doom(neotree)`, when editing the neotree config in doom.
- All Nix sources are pinned with lon; never use channels or `<nixpkgs>` in the config. Machine-specific values (user, host, monitors) are defined once in `configuration.nix`; never repeat them in modules. Details in `docs/NIX.md`.
- Scripts use `#!/usr/bin/env <interpreter>` shebangs (there is no `/usr/bin/bash` on NixOS), are executable and are called directly, not via `sh script.sh`.
- Documentation: every fact has one home. What code does is explained in a comment next to it; why and how things fit together across files is in `docs/<topic>.md`; working rules are in this file. Docs point to the code for concrete values (versions, names, colors, lists) instead of copying them. If a commit changes something that a doc explains, it updates that doc in the same commit.
- New Hyprland keybindings follow the scheme in the comment block of `hyprland.lua` and must work on a German keyboard layout.

- Hyprland's Lua API is new and little documented. Verify functions and arguments against the Hyprland source instead of guessing from hyprlang syntax (see `docs/DESKTOP.md`).
