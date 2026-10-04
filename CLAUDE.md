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
- `.config/nvim` and `.config/xmonad` are git submodules.

## Layout

- `nix/configuration.nix`: main NixOS config, imports the modules below.
- `nix/modules/desktop/`: exactly one desktop module is imported at a time (currently `hyprland.nix`; `xmonad.nix` is the old X11 setup). Switching needs a reboot.
- `nix/modules/hardware/`: hardware setup shared by all desktops (NVIDIA).
- `nix/packages/`: custom package derivations.
- `nix/hardware-configuration.nix` is machine-specific and intentionally not in the repo.
- `.config/hypr/hyprland.lua`: Hyprland config in Lua (not the old hyprlang `.conf` format).
- `bin/`: helper scripts called from Hyprland (`~/bin`).

## Commands

- Apply the NixOS config (on Linux): `pb-nixos-rebuild-switch`, i.e. `sudo nixos-rebuild -I nixos-config=$HOME/nix/configuration.nix switch`.
- Check the Hyprland config (on Linux): `make test` in `.config/hypr`.
- Syntax checks that work anywhere: `nix-instantiate --parse <file>.nix`, `shellcheck bin/*.sh`, `nixfmt` for formatting.

## Conventions

- Commit messages: `<area>: <short lowercase summary>`, e.g. `nix: ...`, `hypr: ...`, `zsh: ...`, `bin: ...`. One logical change per commit. Optionally, add sub-area in brackets like `<area>(<subarea>): ...` such as `doom(neotree)`, when editing the neotree config in doom.
- nixpkgs comes from channels. Packages from unstable are used via the `unstable` module argument (e.g. `unstable.hyprland`).
- Scripts use `#!/usr/bin/env <interpreter>` shebangs (there is no `/usr/bin/bash` on NixOS), are executable and are called directly, not via `sh script.sh`.
- Keyboard layout is German (`de`) with Caps Lock as Escape.
- Hyprland keybinding scheme (`mod` = Super), keep new binds consistent with it:
  - `mod + h/l`: previous/next workspace; `mod + j/k`: cycle windows; `mod + 1-5`: go to workspace
  - `mod + CTRL + hjkl`: focus in a direction
  - `mod + ALT + hjkl`: resize
  - `mod + SHIFT + h/l` and `mod + SHIFT + 1-5`: move window to workspace
  - `mod + SHIFT + CTRL + hjkl`: move window in a direction
  - Single letters launch apps (`t` terminal, `e` emacs, `f` firefox, ...), `mod + SPACE` opens rofi.

## Things to know

- Hyprland's Lua API is new and little documented. Verify functions and arguments against the Hyprland source (`src/config/lua/bindings/`) or the example config instead of guessing from hyprlang syntax.
- greetd logs in directly to Hyprland without a login screen. This is intended.
- Secure Boot via lanzaboote is disabled for now; its config is kept commented out in `configuration.nix`, pinned via lon (`nix/lon.nix`, `nix/lon.lock`).

## Plans and Ideas

I would like to tackle these future goals one by one.

- Setup of graphical toolkits for configuring:
  - wireless network connections
  - bluetooth connections
  - sound
- A bar such as Waybar is not really necessary. Instead, I would prefer, if one could launch a graphical interface for system settings and diagnostics via a keybinding and rofi.
- I am not sure whether rofi is the best launcher for me yet. There are also alternatives out there. What I would like to do is to use it to run smaller terminal commands or start apps, or go into my GUI settings.
- Consistent looks: As of now, all graphical programs are themed differently: kitty, Doom Emacs, thunar, rofi, wlogout ...; For Doom Emacs, this is fine for now but the rest of the system should be consistent. I would like to create a consistent style and start ricing my desktop experience a bit. I am particularly annoyed by the inconsistent configuration of thunar and how it looks as of now. I have very little experience in configuring these toolkits. It seems stylix could be an option. I would like to create a theme from the following colors, and start with it as a baseline. Later, I would like to be able to switch themes.
  - First theme colors: "#ffe017" "#5391fc"  "#4f4848" "#e3574d". If possible, this should be a light theme but a dark version could also be nice.
- Regarding nix flakes: I have no experience with nix flakes and it want to migrate to nix flakes only if it really pays off. There must be a clear and strong reason to migrate.
