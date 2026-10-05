# Dotfiles

Personal dotfiles for a NixOS desktop (host `perry`) with Hyprland. Navigation and usage follow vim keybindings wherever possible. The look should be simple but modern. The main editor is Doom Emacs.

## Design Philosophy

- Any persistent settings or setup should stay as text files.
- System configuration is declarative and reproducible.
- Developing the dotfiles should be a learning experience.
- Doom Emacs and the terminal are the main ways to interact with the system.

## Repository setup

- This is a bare git repository. On the Linux machine, the work tree is `$HOME` and the git directory is `~/.myconfig.git`, used through the `config` function in `.zshrc` (`config status`, `config add`, ...). Paths in this repo are paths relative to `$HOME`.
- The repo is usually edited from a normal clone on macOS. NixOS and Hyprland cannot be built or run there, so changes are tested on the Linux machine.
- A few files are for macOS only (`.config/aerospace/`, `zsh/mac.zsh`); `.zshrc` is shared by both systems.
- `.config/nvim` is a git submodule.

## Layout

- `nix/`: the NixOS configuration, see `docs/NIX.md` (structure, pinning, rebuilding, upgrading).
- `nix/modules/home.nix` and `nix/modules/home/`: home-manager, only for programs that exist on perry alone. `home.nix` lists them, one file each in `nix/modules/home/`. Everything else stays a plain, portable dotfile. Which programs are managed and why is documented in `docs/STYLE.md` only; refer to it instead of repeating the list.
- `docs/STYLE.md`: documentation of the styling architecture, what home-manager manages, how to change the theme and pitfalls.
- `nix/modules/style.nix`: all Nix-based styling (stylix), see `docs/STYLE.md`.
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
- Any commit that changes styling (colors, fonts, themes, which programs are themed or how) must update `docs/STYLE.md` in the same commit.
- Keyboard layout is German (`de`) with Caps Lock as Escape.
- New Hyprland keybindings follow the scheme in the comment block of `hyprland.lua`.

## Things to know

- Hyprland's Lua API is new and little documented. Verify functions and arguments against the Hyprland source instead of guessing from hyprlang syntax (see `docs/DESKTOP.md`).

## Plans and Ideas

I would like to tackle these future goals one by one. Finished goals are removed from this list; what they set up is described in the sections above and in `docs/STYLE.md`.

### Open

- Todo: replace the `-I nixpkgs=...` workaround in `pb-nixos-rebuild-switch` (`zsh/nix.zsh`) with `system.nix`, the entry point added in NixOS 26.05 for building NixOS without channels (see the 26.05 release notes, "Highlights"). A `nix/system.nix` would import the pinned nixpkgs from `lon.nix` and evaluate `configuration.nix` with it, so the rebuild no longer needs to pass the nixpkgs path. Only possible once perry runs 26.05, as older `nixos-rebuild` versions do not know `system.nix`. Check then how `nixos-rebuild` finds the file (e.g. `--file`) and whether the pin warning in `configuration.nix` is still needed.
- Todo: clicking a workspace in waybar does nothing with the Lua Hyprland config; waiting for a waybar release that contains the fix (see docs/STYLE.md, "Known limitation"). Check after each waybar update.
- Todo: the CPU module in waybar always showed 0%, so it was removed from the bar (add `cpu` to `modules-right` in `nix/modules/home/waybar.nix` again once fixed). Not investigated yet. Start with `head -1 /proc/stat` (do the counters change?) and `waybar -l debug` (does the cpu module log an error?).
- I am not sure whether rofi is the best launcher for me yet. There are also alternatives out there. What I would like to do is to use it to run smaller terminal commands or start apps, or go into my GUI settings.
- Consistent looks, step by step. The baseline (base16 scheme One Light via stylix) is in place, see `docs/STYLE.md` for what is themed. Still open: style the shell prompt and then enable the stylix colors in kitty; a custom theme from the colors below (a light theme, if possible; a dark version would be nice) and a way to switch themes. Doom Emacs keeps its own theme.
  - First theme colors: "#ffe017" "#5391fc"  "#4f4848" "#e3574d".

### Decisions

- Waybar stays a slim status bar, as the easiest and most robust solution for now. Building an own system status program is a possible later step. The previously planned "settings and diagnostics via rofi" is parked; the Wi-Fi rofi menu on `mod + n` may become part of it.
- Nix flakes: I have no experience with nix flakes and want to migrate to them only if it really pays off. There must be a clear and strong reason to migrate.
