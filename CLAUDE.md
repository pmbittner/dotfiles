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

- `nix/configuration.nix`: main NixOS config, imports the modules below.
- `nix/modules/desktop/`: exactly one desktop module is imported at a time (currently `hyprland.nix`; the old xmonad setup was removed and is in the git history). Switching needs a reboot.
- `nix/modules/hardware/`: hardware setup shared by all desktops (NVIDIA).
- `nix/modules/home.nix` and `nix/modules/home/`: home-manager, only for programs that exist on perry alone. `home.nix` lists them, one file each in `nix/modules/home/`. Everything else stays a plain, portable dotfile. Which programs are managed and why is documented in `docs/STYLE.md` only; refer to it instead of repeating the list.
- `docs/STYLE.md`: documentation of the styling architecture, what home-manager manages, how to change the theme and pitfalls.
- `nix/modules/style.nix`: all Nix-based styling. Stylix with the base16 scheme One Light, cursor, icons, monospace font (JetBrains Mono Nerd Font), and which stylix targets are enabled. To switch themes, change `base16Scheme` there.
- `nix/modules/files.nix`: mounting (udisks, gvfs), file manager (Thunar) and default programs for file types.
- `nix/modules/network.nix`, `bluetooth.nix`, `sound.nix`: one small module per peripheral topic, shared by all desktops. Each names the graphical tool used for it (see "Desktop tools").
- `nix/packages/`: custom package derivations.
- `nix/hardware-configuration.nix` is machine-specific and intentionally not in the repo.
- `.config/hypr/hyprland.lua`: Hyprland config in Lua (not the old hyprlang `.conf` format).
- `.config/networkmanager-dmenu/config.ini`: config of the rofi Wi-Fi menu.
- `bin/`: helper scripts called from Hyprland (`~/bin`).
- `.zshrc` and `zsh/`: `.zshrc` sets up oh-my-zsh and p10k and sources one file per topic from `zsh/` (`git`, `nix`, `emacs`, `files`, `media`, `system`, `apps`, `dev`, and `mac.zsh` on macOS). New functions and aliases go into the matching topic file; machine-specific ones into the untracked `~/.local.zsh`.

## Commands

- Apply the NixOS config (on Linux): `pb-nixos-rebuild-switch`, i.e. `sudo nixos-rebuild -I nixos-config=$HOME/nix/configuration.nix switch`.
- Check the Hyprland config (on Linux): `make test` in `.config/hypr`.
- Syntax checks that work anywhere: `nix-instantiate --parse <file>.nix`, `shellcheck bin/*.sh`, `nixfmt` for formatting.

## Conventions

- Commit messages: `<area>: <short lowercase summary>`, e.g. `nix: ...`, `hypr: ...`, `zsh: ...`, `bin: ...`. One logical change per commit. Optionally, add sub-area in brackets like `<area>(<subarea>): ...` such as `doom(neotree)`, when editing the neotree config in doom.
- nixpkgs comes from channels. Packages from unstable are used via the `unstable` module argument (e.g. `unstable.hyprland`).
- The user name is defined once in `configuration.nix` and passed to all modules as the `username` argument; do not write `paul` in modules. The machine name (`perry`) is also defined once there; modules that need it use `config.networking.hostName`.
- Scripts use `#!/usr/bin/env <interpreter>` shebangs (there is no `/usr/bin/bash` on NixOS), are executable and are called directly, not via `sh script.sh`.
- Any commit that changes styling (colors, fonts, themes, which programs are themed or how) must update `docs/STYLE.md` in the same commit.
- Keyboard layout is German (`de`) with Caps Lock as Escape.
- Hyprland keybinding scheme (`mod` = Super), keep new binds consistent with it:
  - `mod + h/l`: previous/next workspace; `mod + j/k`: cycle windows; `mod + 1-5`: go to workspace
  - `mod + CTRL + hjkl`: focus in a direction
  - `mod + ALT + hjkl`: resize
  - `mod + SHIFT + h/l` and `mod + SHIFT + 1-5`: move window to workspace
  - `mod + SHIFT + CTRL + hjkl`: move window in a direction
  - Single letters launch apps (`t` terminal, `e` emacs, `f` firefox, ...), `mod + SPACE` opens rofi, `mod + n` opens the Wi-Fi menu.

## Desktop tools

- Wi-Fi/LAN: NetworkManager with wpa_supplicant (switching to iwd was considered and rejected: large change, risky for enterprise Wi-Fi). `networkmanager_dmenu` (rofi menu, `mod + n` or click on waybar's network module) to connect; `nm-connection-editor` (opened from that menu) to add, edit and delete saved networks. LAN is preferred automatically by NetworkManager.
- Bluetooth: BlueZ with `blueman-manager` (click on waybar's bluetooth module, which stays visible and grey when Bluetooth is off). The tray applet is not used.
- Sound: PipeWire (with PulseAudio compatibility) with `pavucontrol` (click on waybar's sound module). `pwvucontrol` was considered; pavucontrol was chosen for robustness and easy GTK3 theming.
- Waybar (config in `nix/modules/home/waybar.nix`): slim, floating, top. Main monitor `DP-4` shows workspaces, clock, RAM, temperature, network, Bluetooth and sound; left monitor `DP-2` shows workspaces only. No tray; add the `tray` module only if a tool really needs it.

## Things to know

- Hyprland's Lua API is new and little documented. Verify functions and arguments against the Hyprland source (`src/config/lua/bindings/`) or the example config instead of guessing from hyprlang syntax.
- greetd logs in directly to Hyprland without a login screen. This is intended.
- lon pins everything that is not in nixpkgs (`nix/lon.lock` is the lock file, `nix/lon.nix` is generated, do not edit it): stylix and home-manager (`release-25.11`) and lanzaboote (frozen). Their modules are imported in `configuration.nix` only. Update the pins with `pb-nixos-update-pins` (runs `lon update`), then rebuild and commit `nix/lon.lock`. lon is independent of Secure Boot.
- Secure Boot via lanzaboote is disabled for now; its config is kept commented out in `configuration.nix`.

## Plans and Ideas

I would like to tackle these future goals one by one. Finished goals are removed from this list; what they set up is described in the sections above and in `docs/STYLE.md`.

### Open

- Todo: clicking a workspace in waybar does nothing with the Lua Hyprland config; waiting for a waybar release that contains the fix (see docs/STYLE.md, "Known limitation"). Check after each waybar update.
- Todo: the CPU module in waybar always showed 0%, so it was removed from the bar (add `cpu` to `modules-right` in `nix/modules/home/waybar.nix` again once fixed). Not investigated yet. Start with `head -1 /proc/stat` (do the counters change?) and `waybar -l debug` (does the cpu module log an error?).
- I am not sure whether rofi is the best launcher for me yet. There are also alternatives out there. What I would like to do is to use it to run smaller terminal commands or start apps, or go into my GUI settings.
- Consistent looks, step by step. The baseline (base16 scheme One Light via stylix) is in place, see `docs/STYLE.md` for what is themed. Still open: style the shell prompt and then enable the stylix colors in kitty; a custom theme from the colors below (a light theme, if possible; a dark version would be nice) and a way to switch themes. Doom Emacs keeps its own theme.
  - First theme colors: "#ffe017" "#5391fc"  "#4f4848" "#e3574d".

### Decisions

- Waybar stays a slim status bar, as the easiest and most robust solution for now. Building an own system status program is a possible later step. The previously planned "settings and diagnostics via rofi" is parked; the Wi-Fi rofi menu on `mod + n` may become part of it.
- Nix flakes: I have no experience with nix flakes and want to migrate to them only if it really pays off. There must be a clear and strong reason to migrate.
