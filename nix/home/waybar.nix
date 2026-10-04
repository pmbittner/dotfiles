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
        modules-right = [ "cpu" "memory" "temperature" "network" "bluetooth" "pulseaudio" ];

        "hyprland/workspaces" = {
          format = "{name}";
        };

        clock = {
          format = "{:%H:%M}";
          format-alt = "{:%A, %d. %B %Y}";
          tooltip-format = "<tt>{calendar}</tt>";
        };

        cpu = {
          format = "CPU {usage}%";
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

        # Click opens the same rofi menu as mod+n.
        network = {
          format-wifi = "WLAN {signalStrength}%";
          format-ethernet = "LAN";
          format-disconnected = "offline";
          tooltip-format-wifi = "{essid} ({signalStrength}%)\n{ipaddr}";
          tooltip-format-ethernet = "{ifname}\n{ipaddr}";
          on-click = "networkmanager_dmenu";
        };

        # Click opens blueman-manager (see nix/modules/bluetooth.nix).
        bluetooth = {
          on-click = "blueman-manager";
          format = "BT";
          format-off = "BT off";
          format-disabled = "";
          format-connected = "BT {num_connections}";
          tooltip-format-connected = "{device_enumerate}";
        };

        # Works through pipewire-pulse. Scroll changes the volume, click opens
        # pavucontrol (see nix/modules/sound.nix).
        pulseaudio = {
          on-click = "pavucontrol";
          format = "VOL {volume}%";
          format-muted = "VOL muted";
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

      .modules-left,
      .modules-center,
      .modules-right {
        background: alpha(@base00, 0.92);
        color: @base05;
        border-radius: 10px;
        padding: 2px 10px;
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

      #temperature.critical {
        color: @base08;
      }

      #network.disconnected,
      #pulseaudio.muted,
      #bluetooth.off {
        color: @base04;
      }
    '';
  };
}
