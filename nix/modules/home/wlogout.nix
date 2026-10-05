{ config, pkgs, ... }:
let
  # Colors come from stylix. It has no wlogout target, so the style is ours.
  c = config.lib.stylix.colors.withHashtag;
  icons = "${pkgs.wlogout}/share/wlogout/icons";
  icon = name: ''
    #${name} {
      background-image: image(url("${icons}/${name}.png"));
    }
  '';
in
{
  # Logout/shutdown menu, opened with mod+ESCAPE via bin/wlogout-once.sh.
  # The layout is not set here, so wlogout's default buttons are used.
  programs.wlogout = {
    enable = true;
    # The button icons are white, so the buttons stay dark even in a light theme.
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
        color: ${c.base00};
        background-color: ${c.base05};
        background-repeat: no-repeat;
        background-position: center;
        background-size: 25%;
      }

      button:focus,
      button:active,
      button:hover {
        background-color: ${c.base0D};
        outline-style: none;
      }

      ${builtins.concatStringsSep "\n" (map icon [ "lock" "logout" "suspend" "hibernate" "shutdown" "reboot" ])}
    '';
  };
}
