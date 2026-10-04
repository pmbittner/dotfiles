{ pkgs, ... }:
{
  # Slim, floating status bar. Hyprland autostarts waybar (see hyprland.lua).
  # Two bars:
  #   - main monitor: workspaces | clock | system status
  #   - left monitor: workspaces only
  #
  # Colors and font come from stylix (see ../modules/style.nix), which defines
  # the CSS colors @base00 ... @base0F. The layout CSS below is our own.
  programs.waybar = {
    enable = true;
    package = pkgs.waybar.overrideAttrs (oldAttrs: {
      mesonFlags = oldAttrs.mesonFlags ++ [ "-Dexperimental=true" ];
    });

    settings = [
      {
        name = "main";
        output = "DP-4";
        layer = "top";
        position = "top";
        margin-top = 10;
        margin-left = 20;
        margin-right = 20;
        spacing = 8;

        modules-left = [ "hyprland/workspaces" ];
        modules-center = [ "clock" ];
        modules-right = [ "memory" "temperature" "network" "bluetooth" "pulseaudio" ];

        "hyprland/workspaces" = {
          format = "{name}";
        };

        clock = {
          format = "{:%H:%M}";
          format-alt = "{:%A, %d. %B %Y}";
          tooltip-format = "<tt>{calendar}</tt>";
        };

        memory = {
          format = "RAM {percentage}%";
        };

        # Uses the default thermal zone. If it shows the wrong value, find the
        # CPU sensor on perry (`ls /sys/class/hwmon/*/name`) and set "hwmon-path".
        temperature = {
          critical-threshold = 80;
          format = "{temperatureC}°C";
        };

        # Icons are Nerd Font glyphs (JetBrainsMono Nerd Font, see style.nix):
        # wifi, ethernet and wifi-off. Details are in the tooltip.
        # Click opens the same rofi menu as mod+n.
        network = {
          format-wifi = "󰖩";
          format-ethernet = "󰈀";
          format-disconnected = "󰖪";
          tooltip-format-wifi = "{essid} ({signalStrength}%)\n{ipaddr}";
          tooltip-format-ethernet = "{ifname}\n{ipaddr}";
          on-click = "networkmanager_dmenu";
        };

        # Click opens blueman-manager (see nix/modules/bluetooth.nix), where
        # bluetooth can be switched on again. The module stays visible when
        # bluetooth is off or disabled; waybar adds the state as a css class
        # (off, disabled, on, connected), which is used for the colors below.
        bluetooth = {
          on-click = "blueman-manager";
          format = "󰂯";
          format-off = "󰂲";
          format-disabled = "󰂲";
          format-connected = "󰂱 {num_connections}";
          tooltip-format-connected = "{device_enumerate}";
        };

        # Works through pipewire-pulse. Scroll changes the volume, click opens
        # pavucontrol (see nix/modules/sound.nix).
        pulseaudio = {
          on-click = "pavucontrol";
          format = "{icon} {volume}%";
          format-muted = "󰖁";
          format-icons = [ "󰕿" "󰖀" "󰕾" ];
        };
      }
      {
        name = "left";
        output = "DP-2";
        layer = "top";
        position = "top";
        margin-top = 10;
        margin-left = 20;
        margin-right = 20;

        modules-left = [ "hyprland/workspaces" ];

        "hyprland/workspaces" = {
          format = "{name}";
        };
      }
    ];

    # Appended after the colors and font that stylix adds.
    style = ''
      * {
        border: none;
        border-radius: 0;
        min-height: 0;
      }

      /* Floating look: the bar itself is transparent, each group is a rounded pill. */
      window#waybar {
        background: transparent;
      }

      /* Each group is a pill. The left bar (second monitor) only has a left
         group, so center and right are limited to the main bar. Otherwise
         the empty groups would show up as small empty pills. The bar name
         from the settings is a css class of the window. */
      window#waybar .modules-left,
      window#waybar.main .modules-center,
      window#waybar.main .modules-right {
        background: alpha(@base00, 0.92);
        color: @base05;
        border-radius: 10px;
        /* The bar window is only as high as its content, so tall icon glyphs
           (like the wifi icon) are cut off without vertical room here. */
        padding: 4px 10px;
      }

      tooltip {
        background: @base00;
        border: 1px solid @base0D;
        border-radius: 6px;
      }

      tooltip label {
        color: @base05;
      }

      #workspaces button {
        padding: 0 6px;
        color: @base04;
        background: transparent;
      }

      #workspaces button.active {
        color: @base0D;
        font-weight: bold;
      }

      #workspaces button:hover {
        background: alpha(@base0D, 0.15);
      }

      /* Icon modules: larger, with padding so they are easy to click. */
      #network,
      #bluetooth,
      #pulseaudio {
        font-size: 14pt;
        padding: 0 8px;
      }

      #network:hover,
      #bluetooth:hover,
      #pulseaudio:hover {
        background: alpha(@base0D, 0.15);
        border-radius: 6px;
      }

      #temperature.critical {
        color: @base08;
      }

      #network.disconnected,
      #pulseaudio.muted,
      #bluetooth.off,
      #bluetooth.disabled {
        color: @base04;
      }

      #bluetooth.on,
      #bluetooth.connected {
        color: @base0D;
      }
    '';
  };
}
