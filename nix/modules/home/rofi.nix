{ config, lib, ... }:
let
  inherit (config.lib.formats.rasi) mkLiteral;
  stylix = config.stylix;

  # The icon theme that stylix uses for GTK apps (Papirus or Papirus-Dark,
  # depending on the theme's polarity), so the icons fit the theme.
  iconTheme = if stylix.polarity == "dark" then stylix.icons.dark else stylix.icons.light;
in
{
  # Launcher and dmenu replacement (mod+SPACE, the wifi menu on mod+n and
  # the theme menu). Colors come from stylix (see ../style.nix); the layout
  # below is our own and is merged with stylix's theme.
  programs.rofi = {
    enable = true;

    # The font of stylix, but larger than its size for popups.
    font = lib.mkForce "${stylix.fonts.monospace.name} 14";

    extraConfig = {
      show-icons = true;
      icon-theme = iconTheme;
    };

    theme = {
      window = {
        border = mkLiteral "2px";
        border-color = mkLiteral "@blue";
        border-radius = mkLiteral "12px";
        padding = mkLiteral "12px";
      };

      inputbar = {
        padding = mkLiteral "8px 12px";
        border-radius = mkLiteral "8px";
        background-color = mkLiteral "@lightbg";
        children = map mkLiteral [ "prompt" "textbox-prompt-colon" "entry" ];
        spacing = mkLiteral "6px";
      };
      # The input bar's children take its background.
      "prompt, textbox-prompt-colon, entry".background-color = mkLiteral "inherit";

      listview = {
        border = mkLiteral "0";
        margin = mkLiteral "10px 0 0 0";
        spacing = mkLiteral "4px";
        lines = 8;
        fixed-height = false; # shrink for short lists, e.g. the theme menu
      };

      element = {
        padding = mkLiteral "8px 12px";
        spacing = mkLiteral "12px";
        border-radius = mkLiteral "8px";
      };

      element-icon.size = mkLiteral "1.4em";
      element-text.vertical-align = mkLiteral "0.5";
    };
  };
}
