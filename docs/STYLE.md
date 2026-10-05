# Styling architecture

How the desktop on `perry` gets its colors, fonts, cursor and icons, why it is built this way, and how to change it.

**Keep this file up to date:** a commit that changes how styling works (which programs are themed, how they get their colors, new pitfalls) updates this file in the same commit. Changing values, such as the scheme or a font, needs no update here.

## Overview

- **One color scheme drives everything:** a [base16](https://github.com/tinted-theming/schemes) scheme, set as `base16Scheme` in `style.nix`.
- **Stylix** turns the scheme into configuration for programs. It is configured in one place: [`nix/modules/style.nix`](../nix/modules/style.nix).
- **Home-manager** is the way stylix reaches most programs. Stylix generates a program's config, and home-manager writes the result into `~/.config`. Therefore a program can only be themed by stylix if home-manager manages it (the list is in [`nix/modules/home.nix`](../nix/modules/home.nix), one file per program in [`nix/modules/home/`](../nix/modules/home/)).
- **Everything else stays a plain dotfile**, so this repository still works on machines without Nix (the Mac, other Linux machines).
- **stylix and home-manager are pinned with lon**, like nixpkgs itself, and must be on the same release as nixpkgs. See [NIX.md](NIX.md).

```
nix/modules/style.nix              stylix: scheme, polarity, fonts, cursor, icons, enabled targets
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
| wlogout | yes (`nix/modules/home/wlogout.nix`) | **no stylix target exists.** The style is written by hand in `wlogout.nix`, using stylix's colors (`config.lib.stylix.colors`). |
| Hyprland (`.config/hypr/hyprland.lua`) | **partly:** the dotfile stays plain, home-manager only generates a values file | `nix/modules/home/hyprland.nix` generates `~/.config/hypr/nix/generated.lua` with the window border colors (active `base0D`, inactive `base03`), the shadow (`base05` at 25% opacity, a soft shadow that suits a light theme), the monitor names and the keyboard layout. `hyprland.lua` loads it with `pcall(require, "nix.generated")`; without the file (other machines) it uses its own defaults (One Light). |
| kitty (`.config/kitty/`) | **partly:** the dotfile `kitty.conf` stays plain, home-manager only generates a colors file | `nix/modules/home/kitty.nix` generates `~/.config/kitty/nix/stylix.conf` from the stylix colors. **The include is currently disabled** (commented out in `kitty.conf`) until the shell prompt is styled, too, so kitty still uses its default theme (`themes/catppuccin_latte.conf`) on `perry`. Once enabled, `kitty.conf` includes the file with `globinclude` after the default theme, so it overrides the default on `perry`. On other machines the file does not exist and the default theme stays. |
| Doom Emacs, ranger, nvim, zsh, ... | no | their own themes, intentionally separate |

Why this split:

- **Portability:** kitty, Doom, ranger, zsh and nvim are used on other machines too. Generating their config with Nix would break that, so they remain plain files. Kitty only gets an optional generated colors file, see the kitty bullet below.
- **Stylix's design:** for most programs, stylix only works through home-manager (the `hm.nix` files in its `modules/` directory). A hand-edited dotfile in `~/.config` would either be ignored by stylix or collide with the generated file. You cannot have both stylix automation and a plain dotfile for the same program.
- **Home-manager only where it pays off:** it is used only for programs that exist on `perry` alone. We do not move other configs into it just because we can.

## How to change the theme

1. Open [`nix/modules/style.nix`](../nix/modules/style.nix).
2. **Use an existing scheme:** change the file name in `base16Scheme`, e.g. `one-dark.yaml`. The available names are the files in `${pkgs.base16-schemes}/share/themes/`, or browse [tinted-theming/schemes](https://github.com/tinted-theming/schemes) (`base16/` directory). Set `polarity` to `"light"` or `"dark"` to match, as it decides the GTK light/dark mode.
3. **Use a custom scheme:** write a base16 YAML file (16 colors `base00`..`base0F`, see the table below) in this repository, e.g. `nix/themes/mine.yaml`, and point `base16Scheme` at it with a path (`./../themes/mine.yaml`). Stylix accepts a path to a scheme file (it also accepts a YAML string or an attribute set). To only tweak single colors of an existing scheme, set `stylix.override = { base0D = "4078f2"; };` instead (values without `#`). (This is the plan for the custom theme from the colors listed in `CLAUDE.md`.)
4. Rebuild with `pb-nixos-rebuild-switch`.
5. Make running programs pick up the change:
   - waybar: `pkill waybar; waybar &` (it is only started once per Hyprland session)
   - dunst: `dunstctl reload`
   - GTK apps and wlogout/rofi: just reopen them
   - cursor: usually needs a new Hyprland session (log out and in)
6. Update this file if the change is more than a different scheme.

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
- **wlogout icons are white.** On a light theme the buttons are therefore dark grey, so the icons stay visible. A dark theme would allow light buttons only if the icons are replaced.
- **Hyprland's generated values file:** Hyprland follows the theme through `~/.config/hypr/nix/generated.lua` (see the table above). After a rebuild, run `hyprctl reload` (autoreload is disabled) and check with `make test` in `.config/hypr` on `perry`. Change colors, monitors or the keyboard layout in Nix, never in `generated.lua` or in the defaults of `hyprland.lua` (those only matter on machines without Nix). Like kitty's, the generated `~/.config/hypr/nix/` is untracked in the dotfiles repo; do not add it.
- **Kitty's include trick:** `kitty.conf` has a `globinclude nix/stylix.conf` line after the default theme include. It is commented out for now (see the table above); uncomment it to let kitty follow the stylix theme. A `globinclude` that matches no file is silent, while a plain `include` of a missing file logs an error on every start; so do not change it to `include`. Kitty versions that do not know `globinclude` show a startup warning, but still start. Do not use `programs.kitty` or the stylix kitty target, as they would generate the whole `kitty.conf` and collide with the dotfile. The generated file overrides every color it sets, so to use a different kitty theme on `perry`, change the theme in `nix/modules/style.nix` instead of the include in `kitty.conf`. The generated `~/.config/kitty/nix/` is untracked in the dotfiles repo; do not add it.
- **`programs.dconf.enable = true`** in `configuration.nix` is needed by the GTK target. Do not remove it. The old manual dconf block was removed on purpose, stylix sets the GTK theme, icons and cursor.
- **Programs started from Hyprland** (waybar, rofi, wlogout) come from the per-user profile of home-manager. If one does not start, check that it is on the `PATH` of the Hyprland session.
