# Plans and ideas

I would like to tackle these future goals one by one. Finished goals are removed from this list; what they set up is described in the other documents in `docs/`.

## Open

- Todo: clicking a workspace in waybar does nothing with the Lua Hyprland config; waiting for a waybar release that contains the fix (see docs/STYLE.md, "Known limitation"). Check after each waybar update.
- Todo: the CPU module in waybar always showed 0%, so it was removed from the bar (add `cpu` to `modules-right` in `nix/modules/home/waybar.nix` again once fixed). Not investigated yet. Start with `head -1 /proc/stat` (do the counters change?) and `waybar -l debug` (does the cpu module log an error?).
- I am not sure whether rofi is the best launcher for me yet. There are also alternatives out there. What I would like to do is to use it to run smaller terminal commands or start apps, or go into my GUI settings.
- Consistent looks, step by step. The baseline (base16 scheme One Light via stylix) is in place, see `docs/STYLE.md` for what is themed. Still open: a custom theme from the colors below (a light theme, if possible; a dark version would be nice), added to `nix/themes.nix`. Later: let Doom Emacs follow the switched theme on NixOS, while keeping their plain dotfiles on other machines (like kitty's color include).
  - First theme colors: "#ffe017" "#5391fc"  "#4f4848" "#e3574d".

## Decisions

- Waybar stays a slim status bar, as the easiest and most robust solution for now. Building an own system status program is a possible later step. The previously planned "settings and diagnostics via rofi" is parked; the Wi-Fi rofi menu on `mod + n` may become part of it.
- Nix flakes: I have no experience with nix flakes and want to migrate to them only if it really pays off. There must be a clear and strong reason to migrate.
