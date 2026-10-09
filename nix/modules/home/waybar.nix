{ osConfig, ... }:
let
  # Monitor names are set in configuration.nix (dotfiles.monitors).
  monitors = osConfig.dotfiles.monitors;

  # Both bars get the same height. Without it, each bar is only as high as its
  # content, so the main bar (larger icons) would be higher than the second one.
  # It must be at least as high as the content of the main bar, otherwise that
  # bar grows beyond this value and the heights differ again.
  barHeight = 40;
in
{
  # Slim, floating status bar. Hyprland autostarts waybar (see hyprland.lua).
  # Two bars:
  #   - main monitor: workspaces | clock | system status
  #   - left monitor: workspaces only
  #
  # Colors and font come from stylix (see ../style.nix), which defines
  # the CSS colors @base00 ... @base0F. The layout CSS below is our own.
  programs.waybar = {
    enable = true;
    # Keep the unmodified package: any override is not in the binary cache
    # and makes waybar compile locally after every update.

    settings = [
      {
        name = "main";
        output = monitors.main;
        layer = "top";
        position = "top";
        height = barHeight;
        margin-top = 10;
        margin-left = 20;
        margin-right = 20;
        spacing = 8;

        modules-left = [ "custom/power" "hyprland/workspaces" ];
        modules-center = [ "clock" ];
        modules-right = [ "memory" "temperature" "network" "bluetooth" "pulseaudio" "custom/theme" ];

        "hyprland/workspaces" = {
          format = "{name}";
        };

        # Logout menu. Uses the same script as the hyprland keybinding
        # (mod+ESCAPE), which toggles wlogout. waybar runs commands with sh, so
        # $HOME is expanded. The icon is the Nerd Font glyph md-power.
        "custom/power" = {
          format = "󰐥";
          tooltip = false;
          on-click = "$HOME/bin/wlogout-once.sh";
        };

        # Theme switcher (bin/theme.sh, see docs/STYLE.md): click opens a
        # rofi menu with all themes; the tooltip shows the current theme.
        # The script runs detached (setsid), because a theme switch reloads
        # waybar, and waybar would otherwise end the script it started.
        "custom/theme" = {
          exec = "$HOME/bin/theme.sh waybar";
          return-type = "json";
          interval = "once";
          on-click = "setsid -f $HOME/bin/theme.sh menu";
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
        output = monitors.left;
        layer = "top";
        position = "top";
        height = barHeight;
        margin-top = 10;
        margin-left = 20;
        margin-right = 20;

        # Only workspaces here. The unused center group is removed.
        no-center = true;
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

      /* The left bar (second monitor) has no modules in the right group. Make
         sure that it takes up no space and draws nothing. */
      window#waybar.left .modules-center,
      window#waybar.left .modules-right {
        background: none;
        border: none;
        box-shadow: none;
        padding: 0;
        margin: 0;
        min-width: 0;
        min-height: 0;
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
        border-radius: 6px;
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
      #custom-power,
      #network,
      #bluetooth,
      #pulseaudio,
      #custom-theme {
        font-size: 14pt;
        padding: 0 8px;
      }

      /* The wifi glyph has more empty space on its left side than on its
         right, so it looks shifted to the right. Moving the padding by 2px
         centers it; adjust the 2px if it still looks off. */
      #network {
        padding: 0 11px 0 5px;
      }

      #custom-power:hover,
      #network:hover,
      #bluetooth:hover,
      #pulseaudio:hover,
      #custom-theme:hover {
        background: alpha(@base0D, 0.15);
        border-radius: 6px;
      }

      /* Divider between the text modules (RAM, temperature) and the icons.
         It is the right border of the temperature module and not the left
         border of the network module, as the latter shifts the network icon
         off-center inside its hover highlight. */
      #temperature {
        border-right: 1px solid alpha(@base03, 0.6);
        padding-right: 8px;
        margin-right: 4px;
      }

      #custom-power {
        color: @base08;
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
