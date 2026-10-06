{ config, pkgs, ... }:
let
  # Colors come from stylix. It has no wlogout target, so the style is ours.
  c = config.lib.stylix.colors.withHashtag;
  hex = config.lib.stylix.colors; # the same colors without '#'

  # wlogout's button icons are white PNGs. They are recolored at build time,
  # so they fit any theme: one set in the text color for normal buttons and
  # one in the background color for the highlighted button.
  recolorIcons =
    color:
    pkgs.runCommand "wlogout-icons-${color}" { nativeBuildInputs = [ pkgs.imagemagick ]; } ''
      mkdir -p $out
      for icon in ${pkgs.wlogout}/share/wlogout/icons/*.png; do
        magick "$icon" -alpha on -fill "#${color}" -colorize 100 "$out/$(basename "$icon")"
      done
    '';
  normalIcons = recolorIcons hex.base05;
  activeIcons = recolorIcons hex.base00;

  buttons = [ "lock" "logout" "suspend" "hibernate" "shutdown" "reboot" ];
  iconStyle = name: ''
    #${name} {
      background-image: image(url("${normalIcons}/${name}.png"));
    }
    #${name}:focus,
    #${name}:active,
    #${name}:hover {
      background-image: image(url("${activeIcons}/${name}.png"));
    }
  '';
in
{
  # Logout/shutdown menu, opened with mod+ESCAPE via bin/wlogout-once.sh.
  # The layout is not set here, so wlogout's default buttons are used.
  programs.wlogout = {
    enable = true;
    style = ''
      @define-color bg ${c.base00};

      * {
        background-image: none;
        box-shadow: none;
      }

      window {
        background-color: alpha(@bg, 0.9);
      }

      button {
        border-radius: 10px;
        border: 2px solid @bg;
        color: ${c.base05};
        background-color: ${c.base01};
        background-repeat: no-repeat;
        background-position: center;
        background-size: 25%;
      }

      button:focus,
      button:active,
      button:hover {
        color: ${c.base00};
        background-color: ${c.base0D};
        outline-style: none;
      }

      ${builtins.concatStringsSep "\n" (map iconStyle buttons)}
    '';
  };
}
