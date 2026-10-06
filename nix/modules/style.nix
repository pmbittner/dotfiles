{ config, lib, pkgs, username, ... }:
let
  # The switchable themes, see ../themes.nix.
  inherit (import ../themes.nix) default themes;
  # A scheme is either the name of a base16-schemes file or a path to an own
  # scheme file in this repository.
  schemeFile =
    theme:
    if builtins.isPath theme.scheme then
      theme.scheme
    else
      "${pkgs.base16-schemes}/share/themes/${theme.scheme}.yaml";
  defaultTheme =
    lib.findFirst (t: t.name == default)
      (throw "themes.nix: default theme '${default}' is not in the list")
      themes;
  otherThemes = lib.filter (t: t.name != default) themes;
in
{
  # All Nix-based styling lives in this file.
  #
  # Stylix generates app themes from one color scheme. Most of its targets run
  # through home-manager, see home.nix for which programs are managed by it.
  # The stylix NixOS module itself is imported in configuration.nix.

  stylix = {
    enable = true;
    # The default theme. The other themes are home-manager specialisations,
    # see below.
    polarity = defaultTheme.polarity;
    base16Scheme = schemeFile defaultTheme;

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

  home-manager.users.${username} = {
    # Stylix themes home-manager programs through its home-manager
    # integration (see home.nix). Targets are enabled per program below; add
    # new ones here.
    stylix.targets = {
      dunst.enable = true;
      gtk.enable = true;
      rofi.enable = true;
      waybar = {
        enable = true;
        # Only colors and font; the layout CSS is in home/waybar.nix.
        addCss = false;
      };
    };

    # One specialisation per non-default theme: the same home-manager config
    # with another scheme. All are built with every rebuild, so switching a
    # theme only runs the specialisation's activation script (bin/theme.sh).
    # Stylix copies the NixOS settings above into home-manager with low
    # priority (mkDefault), so they can be overridden here.
    specialisation = lib.listToAttrs (
      map (theme: {
        inherit (theme) name;
        value.configuration.stylix = {
          inherit (theme) polarity;
          base16Scheme = schemeFile theme;
        };
      }) otherThemes
    );
  };

  # Files for bin/theme.sh: the theme list (`name|label` per line, in menu
  # order), the default theme, and the home-manager generation whose
  # specialisations are the themes.
  environment.etc = {
    "dotfiles/themes".text = lib.concatMapStrings (t: "${t.name}|${t.label}\n") themes;
    "dotfiles/theme-default".text = default + "\n";
    "dotfiles/home-generation".text =
      "${config.home-manager.users.${username}.home.activationPackage}\n";
  };
}
