{ pkgs, ... }:
let
  # stylix and home-manager are pinned with lon (see ../lon.nix, ../lon.lock).
  sources = import ../lon.nix;
in
{
  # All Nix-based styling lives in this file.
  #
  # Stylix generates app themes from one color scheme. Most of its targets run
  # through home-manager, see home.nix for which programs are managed by it.
  imports = [ (import sources.stylix).nixosModules.stylix ];

  stylix = {
    enable = true;
    polarity = "light";
    # To switch themes, change the scheme here (and polarity if needed).
    # All schemes: https://github.com/tinted-theming/schemes (base16/)
    base16Scheme = "${pkgs.base16-schemes}/share/themes/one-light.yaml";

    # Only explicitly enabled targets are themed. Targets are enabled on the
    # home-manager side below, as they are separate from the NixOS ones.
    autoEnable = false;

    fonts.monospace = {
      package = pkgs.nerd-fonts.jetbrains-mono;
      name = "JetBrainsMono Nerd Font";
    };

    cursor = {
      package = pkgs.bibata-cursors;
      name = "Bibata-Modern-Ice";
      size = 24;
    };

    icons = {
      enable = true;
      package = pkgs.papirus-icon-theme;
      light = "Papirus";
      dark = "Papirus-Dark";
    };
  };

  # Stylix themes home-manager programs through its home-manager integration
  # (see home.nix). Targets are enabled per program below; add new ones here.
  home-manager.users.paul.stylix.targets = {
    dunst.enable = true;
    gtk.enable = true;
    rofi.enable = true;
  };
}
