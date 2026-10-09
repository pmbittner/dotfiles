# Styling architecture

How the desktop on `perry` gets its colors, fonts, cursor and icons, why it is built this way, and how to change it.

**Keep this file up to date:** a commit that changes how styling works (which programs are themed, how they get their colors, new pitfalls) updates this file in the same commit. Changing values, such as the scheme or a font, needs no update here.

## Overview

- **One color scheme drives everything:** a [base16](https://github.com/tinted-theming/schemes) scheme, one of the themes in `nix/themes.nix`, which can be switched at runtime (see below).
- **Stylix** turns the scheme into configuration for programs. It is configured in one place: [`nix/modules/style.nix`](../nix/modules/style.nix).
- **Home-manager** is the way stylix reaches most programs. Stylix generates a program's config, and home-manager writes the result into `~/.config`. Therefore a program can only be themed by stylix if home-manager manages it (the list is in [`nix/modules/home.nix`](../nix/modules/home.nix), one file per program in [`nix/modules/home/`](../nix/modules/home/)).
- **Everything else stays a plain dotfile**, so this repository still works on machines without Nix (the Mac, other Linux machines).
- **stylix and home-manager are pinned with lon**, like nixpkgs itself, and must be on the same release as nixpkgs. See [NIX.md](NIX.md).

```
nix/themes.nix                     the switchable themes
nix/modules/style.nix              stylix: default theme, theme specialisations, fonts, cursor, icons, enabled targets
bin/theme.sh                       switches themes at runtime
nix/modules/home.nix               home-manager base + list of managed programs
nix/modules/home/<program>.nix     config of one managed program (see the table below)
```

## What is managed by home-manager, and what is not

| Program | Managed by home-manager? | How it gets its colors |
|---|---|---|
| GTK apps (Thunar, pavucontrol, blueman, ...) | yes | stylix target `gtk` (adw-gtk3 theme + generated CSS, Papirus icons, cursor) |
| rofi (launcher, wifi menu) | yes (`nix/modules/home/rofi.nix`) | stylix target `rofi` |
| dunst (notifications) | yes (`nix/modules/home/dunst.nix`) | stylix target `dunst` |
| waybar | yes (`nix/modules/home/waybar.nix`) | stylix target `waybar` with `addCss = false`: stylix only provides the CSS colors `@base00`..`@base0F` and the font. The layout CSS is our own, in `waybar.nix`. |
| wlogout | yes (`nix/modules/home/wlogout.nix`) | **no stylix target exists.** The style is written by hand in `wlogout.nix`, using stylix's colors (`config.lib.stylix.colors`). Its white icons are recolored at build time with ImageMagick, in the text color and, for the highlighted button, in the background color. |
| Hyprland (`.config/hypr/hyprland.lua`) | **partly:** the dotfile stays plain, home-manager only generates a values file | `nix/modules/home/hyprland.nix` generates `~/.config/hypr/nix/generated.lua` with the window border colors (active `base0D`, inactive `base03`), the shadow (`base05` at 25% opacity, a soft shadow that suits a light theme), the monitor names and the keyboard layout. `hyprland.lua` loads it with `pcall(require, "nix.generated")`; without the file (other machines) it uses its own defaults (One Light). |
| kitty (`.config/kitty/`) | **partly:** the dotfile `kitty.conf` stays plain, home-manager only generates a colors file | `nix/modules/home/kitty.nix` generates `~/.config/kitty/nix/stylix.conf` from the stylix colors. `kitty.conf` includes it with `globinclude` after the default theme (`themes/catppuccin_latte.conf`), so it overrides the default on `perry`. On other machines the file does not exist and the default theme stays. |
| zsh prompt (powerlevel10k, `zsh/p10k.zsh`) | no | the terminal's palette: the prompt uses colors 0-15, which kitty takes from the theme. The generated `~/.p10k.zsh` also uses a few fixed 256-colors; on NixOS (`/etc/NIXOS` exists), `zsh/p10k.zsh` replaces them with palette colors. Other systems keep the prompt as generated. |
| Doom Emacs, ranger, nvim, ... | no | their own themes, intentionally separate |

Why this split:

- **Portability:** kitty, Doom, ranger, zsh and nvim are used on other machines too. Generating their config with Nix would break that, so they remain plain files. Kitty only gets an optional generated colors file, see the kitty bullet below.
- **Stylix's design:** for most programs, stylix only works through home-manager (the `hm.nix` files in its `modules/` directory). A hand-edited dotfile in `~/.config` would either be ignored by stylix or collide with the generated file. You cannot have both stylix automation and a plain dotfile for the same program.
- **Home-manager only where it pays off:** it is used only for programs that exist on `perry` alone. We do not move other configs into it just because we can.

## Themes and switching between them

- **The themes are listed in [`nix/themes.nix`](../nix/themes.nix):** name, label, base16 scheme, polarity, and which one is the default. The order is the order in the menu.
- **All themes are built with every rebuild.** The default theme configures stylix in `style.nix`; every other theme is a home-manager *specialisation*: the same home-manager config with another scheme and polarity. Stylix copies its NixOS settings into home-manager with low priority (`mkDefault`), which is what lets a specialisation override them.
- **Switching** runs the chosen specialisation's activation script, without a rebuild or root rights. `bin/theme.sh` does this. It is used by the theme button in waybar (click: menu) and can be run in a terminal (`theme.sh next`, `theme.sh set one-dark`, ...).
- **Reloading running programs:** home-manager reloads dunst on activation; `theme.sh` reloads waybar (`SIGUSR2`) and Hyprland. Reloading waybar recreates its bars, so the mouse pointer loses the button it is on. That is why the button only opens a menu and does not cycle on scroll.
- **The waybar button starts `theme.sh` detached** (`setsid -f`). waybar ends the processes it started when it reloads, which would stop the script in the middle of a switch.
- **The active theme is remembered** in `~/.local/state/dotfiles/theme` (the output of the last activation is in `activate.log` next to it). A rebuild and a new boot activate the default theme; Hyprland's autostart and `pb-nixos-rebuild-switch` restore the remembered one with `theme.sh restore`.
- **The script finds the themes** through files that `style.nix` writes to `/etc/dotfiles/`: the theme list, the default, and the home-manager generation whose `specialisation/<name>` are the themes.
- **Not switched live:** open GTK apps (reopen them), and programs outside stylix (Doom Emacs). kitty is reloaded by `theme.sh` (`SIGUSR1`), and the zsh prompt in it follows.

### Adding a theme

1. Add an entry to `nix/themes.nix`. For `scheme`, use the name of a scheme from the base16-schemes package (the files in `${pkgs.base16-schemes}/share/themes/`, or browse [tinted-theming/schemes](https://github.com/tinted-theming/schemes), `base16/` directory). Set `polarity` to `"light"` or `"dark"` to match; it decides the GTK light/dark mode and the icon variant.
2. **For an own scheme,** write a base16 YAML file (16 colors `base00`..`base0F`, see the table below) in this repository, e.g. `nix/themes/mine.yaml`, and set `scheme = ./themes/mine.yaml;` (a path, not a string). This is the plan for the custom theme from the colors listed in [PLANS.md](PLANS.md).
3. Rebuild with `pb-nixos-rebuild-switch`. The new theme then shows up in the menu.

To change the default theme, change `default` in `nix/themes.nix` and rebuild.

### What the 16 base16 colors mean

| Color | Role |
|---|---|
| base00 | main background |
| base01 | lighter background (bars, popups) |
| base02 | selection background |
| base03 | comments, disabled, borders |
| base04 | dim foreground |
| base05 | main foreground (text) |
| base06 / base07 | light/dark foreground variants |
| base08 | red (errors, urgent) |
| base09 | orange |
| base0A | yellow |
| base0B | green |
| base0C | cyan |
| base0D | blue (main accent, links, active items) |
| base0E | purple |
| base0F | brown |

In the hand-written CSS (waybar, wlogout) we use these names, never fixed hex values, so a theme change reaches them. Waybar CSS uses `@base0D`, wlogout gets the values interpolated from Nix.

## What to watch out for

- **Stylix targets live on the home-manager side.** Enabling a target at NixOS level (`stylix.targets.x.enable`) does nothing for home-manager programs. Targets are enabled in `style.nix` under `home-manager.users.${username}.stylix.targets`. Missing this once left the GTK theme unset. The check is: `config.home-manager.users.<user>.gtk.theme.name` must not be `null`.
- **`autoEnable = false`.** A new program is only themed when its target is enabled explicitly in `style.nix`. Check that a target exists for the program in stylix's `modules/` directory (programs without one, like wlogout, need hand-written styles using `config.lib.stylix.colors.withHashtag`).
- **Adding a managed program takes three steps:** create `nix/modules/home/<program>.nix`, list it in `nix/modules/home.nix`, enable its stylix target in `style.nix`. If the program was installed system-wide before, remove it from `environment.systemPackages` (home-manager installs it).
- **Generated files are read-only.** Files in `~/.config` that come from home-manager are symlinks into the Nix store. Edit the `.nix` file, not the file in `~/.config`. If a file already existed, home-manager renames it to `<name>.hm-backup` instead of failing; delete such backups once you no longer need them.
- **Installed fonts:** besides the stylix fonts, `fonts.packages` in `configuration.nix` installs a few fonts system-wide. There are no separate icon fonts; all icons come from the Nerd Font.
- **Waybar icons are Nerd Font glyphs** (network, Bluetooth, sound), typed as literal characters in `waybar.nix`, as Nix strings have no `\u` escapes. They only show up while the monospace font in `style.nix` is a Nerd Font. Look up glyphs by name in [glyphnames.json](https://github.com/ryanoasis/nerd-fonts/blob/master/glyphnames.json) (e.g. `md-wifi`; the `char` field of an entry is the glyph to paste).
- **Waybar icon size:** the icon modules (`#network`, `#bluetooth`, `#pulseaudio`) have their own larger `font-size` and padding in `waybar.nix`, which also makes them easy to click. The bar window is only as high as its content, so tall icon glyphs are cut off at its edge if the pills have too little vertical padding. If an icon looks clipped, increase the `padding` of the pill or reduce the icon `font-size`.
- **Waybar divider:** a thin line after `#temperature` separates the text modules from the icons (CSS `border-right`). It must not be a border of an icon module, as that shifts the icon off-center in its hover highlight. A separate pill would need `group` modules instead.
- **Waybar power button:** the leftmost button of the main bar runs `$HOME/bin/wlogout-once.sh`, the same script as the hyprland keybinding. The script must exist in `~/bin` (it is part of the dotfiles, `bin/`).
- **Waybar height:** both bars use the same fixed `barHeight` (top of `waybar.nix`). It has to be at least as high as the content of the main bar (large icons plus pill padding). If the main bar grows beyond it, its height differs from the second bar again; raise `barHeight`, or lower the icon font size.
- **Known limitation, waybar workspace clicks:** clicking a workspace number in waybar does nothing, because released waybar versions (checked up to 0.15.0) send the old IPC dispatch command, which Hyprland with a Lua config does not accept. Waybar's development branch has the fix (`IPC::buildLuaDispatch` in `src/modules/hyprland/backend.cpp`). A local patch worked in principle, but was dropped, since a patched package is not in the binary cache and has to be compiled locally after every update that touches waybar or its dependencies. After a waybar update, check if the fix is released.
- **Waybar:** `addCss = false` is deliberate, as stylix's full CSS would replace the floating pill design. The state classes (e.g. `#bluetooth.off`) come from waybar itself. Each group of modules is a pill, and empty groups would show up as small empty pills. That is why the pill style for the center and right group is limited to the `main` bar (`window#waybar.main`, the bar name is a CSS class); the second monitor's bar only has a left group. The second bar also sets `no-center` and has an explicit CSS reset for its unused groups (`window#waybar.left`). If you add modules to the second bar, extend those selectors and remove the reset for the group you use. The monitor names come from `dotfiles.monitors` in `configuration.nix`; change them there if the monitors change.
- **Hyprland's generated values file:** Hyprland follows the theme through `~/.config/hypr/nix/generated.lua` (see the table above). After a rebuild, run `hyprctl reload` (autoreload is disabled) and check with `make test` in `.config/hypr` on `perry`. Change colors, monitors or the keyboard layout in Nix, never in `generated.lua` or in the defaults of `hyprland.lua` (those only matter on machines without Nix). Like kitty's, the generated `~/.config/hypr/nix/` is untracked in the dotfiles repo; do not add it.
- **Prompt colors:** keep the prompt on the terminal palette. New colors in `~/.p10k.zsh` (e.g. after `p10k configure`) should be numbers 0-15; fixed 256-colors ignore the theme and need an entry in the NixOS block of `zsh/p10k.zsh`. Text on colored segments uses 0 (the theme's background), which reads well in light and dark themes.
- **Kitty's include trick:** `kitty.conf` has a `globinclude nix/stylix.conf` line after the default theme include. A `globinclude` that matches no file is silent, while a plain `include` of a missing file logs an error on every start; so do not change it to `include`. Kitty versions that do not know `globinclude` show a startup warning, but still start. Do not use `programs.kitty` or the stylix kitty target, as they would generate the whole `kitty.conf` and collide with the dotfile. The generated file overrides every color it sets, so to use a different kitty theme on `perry`, change the theme in `nix/modules/style.nix` instead of the include in `kitty.conf`. The generated `~/.config/kitty/nix/` is untracked in the dotfiles repo; do not add it.
- **`programs.dconf.enable = true`** in `configuration.nix` is needed by the GTK target. Do not remove it. The old manual dconf block was removed on purpose, stylix sets the GTK theme, icons and cursor.
- **Programs started from Hyprland** (waybar, rofi, wlogout) come from the per-user profile of home-manager. If one does not start, check that it is on the `PATH` of the Hyprland session.
