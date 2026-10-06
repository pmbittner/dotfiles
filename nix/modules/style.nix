{ pkgs, username, ... }:
{
  # All Nix-based styling lives in this file.
  #
  # Stylix generates app themes from one color scheme. Most of its targets run
  # through home-manager, see home.nix for which programs are managed by it.
  # The stylix NixOS module itself is imported in configuration.nix.

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
  home-manager.users.${username}.stylix.targets = {
    dunst.enable = true;
    gtk.enable = true;
    rofi.enable = true;
    waybar = {
      enable = true;
      # Only colors and font; the layout CSS is in home/waybar.nix.
      addCss = false;
    };
  };
}
