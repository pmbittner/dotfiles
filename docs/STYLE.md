# Styling architecture

How the desktop on `perry` gets its colors, fonts, cursor and icons, why it is built this way, and how to change it.

**Keep this file up to date:** every commit that changes styling (colors, fonts, themes, which programs are themed or how) must update this file in the same commit.

## Overview

- **One color scheme drives everything:** a [base16](https://github.com/tinted-theming/schemes) scheme, currently **One Light** (light polarity).
- **Stylix** turns the scheme into configuration for programs. It is configured in one place: [`nix/modules/style.nix`](../nix/modules/style.nix).
- **Home-manager** is the way stylix reaches most programs. Stylix generates a program's config, and home-manager writes the result into `~/.config`. Therefore a program can only be themed by stylix if home-manager manages it (the list is in [`nix/modules/home.nix`](../nix/modules/home.nix), one file per program in [`nix/home/`](../nix/home/)).
- **Everything else stays a plain dotfile**, so this repository still works on machines without Nix (the Mac, other Linux machines).
- **stylix and home-manager are pinned with lon** (`nix/lon.lock`, branch `release-25.11`, matching nixpkgs). Update them with `pb-nixos-update-pins`, then rebuild.

```
nix/modules/style.nix      stylix: scheme, polarity, fonts, cursor, icons, enabled targets
nix/modules/home.nix       home-manager base + list of managed programs
nix/home/<program>.nix     config of one managed program (rofi, dunst, waybar, wlogout)
nix/lon.lock               pins for stylix and home-manager
```

## What is managed by home-manager, and what is not

| Program | Managed by home-manager? | How it gets its colors |
|---|---|---|
| GTK apps (Thunar, pavucontrol, blueman, ...) | yes | stylix target `gtk` (adw-gtk3 theme + generated CSS, Papirus icons, cursor) |
| rofi (launcher, wifi menu) | yes (`nix/home/rofi.nix`) | stylix target `rofi` |
| dunst (notifications) | yes (`nix/home/dunst.nix`) | stylix target `dunst` |
| waybar | yes (`nix/home/waybar.nix`) | stylix target `waybar` with `addCss = false`: stylix only provides the CSS colors `@base00`..`@base0F` and the font. The layout CSS is our own, in `waybar.nix`. |
| wlogout | yes (`nix/home/wlogout.nix`) | **no stylix target exists.** The style is written by hand in `wlogout.nix`, using stylix's colors (`config.lib.stylix.colors`). |
| Hyprland (`.config/hypr/hyprland.lua`) | no | by hand: **not themed yet** (window border colors are still fixed values) |
| kitty (`.config/kitty/`) | no | by hand: **not themed yet** |
| Doom Emacs, ranger, nvim, zsh, ... | no | their own themes, intentionally separate |

Why this split:

- **Portability:** kitty, Doom, ranger, zsh and nvim are used on other machines too. Generating their config with Nix would break that, so they remain plain files.
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

| Color | Role | One Light value |
|---|---|---|
| base00 | main background | `#fafafa` |
| base01 | lighter background (bars, popups) | `#f0f0f1` |
| base02 | selection background | `#e5e5e6` |
| base03 | comments, disabled, borders | `#a0a1a7` |
| base04 | dim foreground | `#696c77` |
| base05 | main foreground (text) | `#383a42` |
| base06 / base07 | light/dark foreground variants | `#202227` / `#090a0b` |
| base08 | red (errors, urgent) | `#ca1243` |
| base09 | orange | `#d75f00` |
| base0A | yellow | `#c18401` |
| base0B | green | `#50a14f` |
| base0C | cyan | `#0184bc` |
| base0D | blue (main accent, links, active items) | `#4078f2` |
| base0E | purple | `#a626a4` |
| base0F | brown | `#986801` |

In the hand-written CSS (waybar, wlogout) we use these names, never fixed hex values, so a theme change reaches them. Waybar CSS uses `@base0D`, wlogout gets the values interpolated from Nix.

## What to watch out for

- **Stylix targets live on the home-manager side.** Enabling a target at NixOS level (`stylix.targets.x.enable`) does nothing for home-manager programs. Targets are enabled in `style.nix` under `home-manager.users.paul.stylix.targets`. Missing this once left the GTK theme unset. The check is: `config.home-manager.users.paul.gtk.theme.name` must not be `null`.
- **`autoEnable = false`.** A new program is only themed when its target is enabled explicitly in `style.nix`. Check that a target exists for the program in stylix's `modules/` directory (programs without one, like wlogout, need hand-written styles using `config.lib.stylix.colors.withHashtag`).
- **Adding a managed program takes three steps:** create `nix/home/<program>.nix`, list it in `nix/modules/home.nix`, enable its stylix target in `style.nix`. If the program was installed system-wide before, remove it from `environment.systemPackages` (home-manager installs it).
- **Generated files are read-only.** Files in `~/.config` that come from home-manager are symlinks into the Nix store. Edit the `.nix` file, not the file in `~/.config`. If a file already existed, home-manager renames it to `<name>.hm-backup` instead of failing; delete such backups once you no longer need them.
- **Versions must match.** stylix and home-manager must be on the same release branch as nixpkgs (`release-25.11`). On a NixOS upgrade, switch the `branch` of both sources in `nix/lon.lock` to the new release and run `pb-nixos-update-pins`. Never change `home.stateVersion` or `system.stateVersion` for this.
- **Waybar icons are Nerd Font glyphs** (network, Bluetooth, sound), typed as literal characters in `waybar.nix`, as Nix strings have no `\u` escapes. They only show up while the monospace font in `style.nix` is a Nerd Font. Look up glyphs by name in [glyphnames.json](https://github.com/ryanoasis/nerd-fonts/blob/master/glyphnames.json) (e.g. `md-wifi`; the `char` field of an entry is the glyph to paste).
- **Waybar icon size:** the icon modules (`#network`, `#bluetooth`, `#pulseaudio`) have their own larger `font-size` and padding in `waybar.nix`, which also makes them easy to click. The bar window is only as high as its content, so tall icon glyphs are cut off at its edge if the pills have too little vertical padding. If an icon looks clipped, increase the `padding` of the pill or reduce the icon `font-size`.
- **Waybar divider:** a thin line after `#temperature` separates the text modules from the icons (CSS `border-right`). It must not be a border of an icon module, as that shifts the icon off-center in its hover highlight. A separate pill would need `group` modules instead.
- **Waybar power button:** the leftmost button of the main bar runs `$HOME/bin/wlogout-once.sh`, the same script as the hyprland keybinding. The script must exist in `~/bin` (it is part of the dotfiles, `bin/`).
- **Waybar height:** both bars use the same fixed `barHeight` (top of `waybar.nix`). It has to be at least as high as the content of the main bar (large icons plus pill padding). If the main bar grows beyond it, its height differs from the second bar again; raise `barHeight`, or lower the icon font size.
- **Waybar:** `addCss = false` is deliberate, as stylix's full CSS would replace the floating pill design. The state classes (e.g. `#bluetooth.off`) come from waybar itself. Each group of modules is a pill, and empty groups would show up as small empty pills. That is why the pill style for the center and right group is limited to the `main` bar (`window#waybar.main`, the bar name is a CSS class); the second monitor's bar only has a left group. The second bar also sets `no-center` and has an explicit CSS reset for its unused groups (`window#waybar.left`). If you add modules to the second bar, extend those selectors and remove the reset for the group you use. The monitor names (`DP-4` main, `DP-2` left) are hard-coded in `waybar.nix` and must be changed there if the monitors change.
- **wlogout icons are white.** On a light theme the buttons are therefore dark grey, so the icons stay visible. A dark theme would allow light buttons only if the icons are replaced.
- **Hyprland and kitty do not follow the theme yet.** Changing `base16Scheme` does not change their colors. When they are themed, document it in the table above. Their config files are plain dotfiles; the colors have to be copied from the table above by hand.
- **`programs.dconf.enable = true`** in `configuration.nix` is needed by the GTK target. Do not remove it. The old manual dconf block was removed on purpose, stylix sets the GTK theme, icons and cursor.
- **Testing:** the NixOS config cannot be built on the Mac. `nix-instantiate --parse <file>.nix` only checks syntax. Real checks (option names, wrong target level) happen on `perry` with `pb-nixos-rebuild-switch`. If a rebuild fails or something looks wrong after it, `pb-nixos-show-generations` and the boot menu allow going back to the previous generation.
- **Programs started from Hyprland** (waybar, rofi, wlogout) come from the per-user profile of home-manager. If one does not start, check that it is on the `PATH` of the Hyprland session.
