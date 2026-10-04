{ pkgs, unstable, ... }:
{
  # Use Hyprland
  programs.hyprland = {
    enable = true;
    package = unstable.hyprland;
    # Keep the portal in sync with the Hyprland version.
    portalPackage = unstable.xdg-desktop-portal-hyprland;
    xwayland.enable = true;
  };
  # Display Manager
  services.greetd = {
    enable = true;
    settings = {
      default_session = {
        command = "start-hyprland";
        user = "paul";
      };
    };
  };
  # XDG takes care of inter-app communication and link opening and so on.
  # programs.hyprland already adds xdg-desktop-portal-hyprland (portalPackage).
  # The GTK portal adds what the Hyprland portal lacks, e.g. file pickers.
  xdg.portal = {
    enable = true;
    extraPortals = [ pkgs.xdg-desktop-portal-gtk ];
  };
  # Some variables necessary to run Hyprland.
  # Note: WLR_NO_HARDWARE_CURSORS is a wlroots variable that Hyprland ignores.
  # Use cursor.no_hardware_cursors in hyprland.lua instead (default: auto,
  # which already disables hardware cursors on nvidia).
  environment.sessionVariables = {
    # Hint electron apps to use wayland
    NIXOS_OZONE_WL = "1";
  };

  # Sound via pipewire on hyprland
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
    jack.enable = true;
  };

  environment.systemPackages = with pkgs; [
    #### hyprland
    ## Lua LSP
    lua-language-server
    ## Bar
    # I want to try eww as well.
    (waybar.overrideAttrs (oldAttrs: {
      mesonFlags = oldAttrs.mesonFlags ++ [ "-Dexperimental=true" ];
    }))
    ## Notifications
    dunst # for notifications (an alternative would be "mako")
    libnotify
    ## Wallpapers: choose exactly one of
    # hyprpaper
    # swaybg
    # wpaperd
    # mpvpaper
    unstable.awww
    ## Launcher
    rofi
    # shutdown
    wlogout

    adw-gtk3          # Example GTK3 theme (replace with your preferred theme)
    papirus-icon-theme # Example Icon theme

  ];
}
