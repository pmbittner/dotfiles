{ lib, ... }:
{
  # Names of the monitors (Wayland output names, see `hyprctl monitors`).
  # They are set per machine in configuration.nix. Waybar uses them directly;
  # Hyprland gets them through a generated file (see home/hyprland.nix).
  # Home-manager modules read them as `osConfig.dotfiles.monitors`.
  options.dotfiles.monitors = {
    main = lib.mkOption {
      type = lib.types.str;
      description = "Output name of the main monitor (workspaces 1-5, main waybar).";
    };
    left = lib.mkOption {
      type = lib.types.str;
      description = "Output name of the monitor left of the main one (workspace F, Firefox).";
    };
  };
}
