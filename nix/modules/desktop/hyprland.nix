{ pkgs, unstable, ... }:
{
  services.xserver.videoDrivers = [ "nvidia" ];

  # Use Hyprland
  programs.hyprland = {
    enable = true;
    package = unstable.hyprland;
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
  xdg.portal = {
    enable = true;
    # extraPortals = [
    #   pkgs.xdg-desktop-portal-hyprland
    #   pkgs.xdg-desktop-portal-gtk
    # ];
  };
  # Some variables necessary to run Hyprland.
  environment.sessionVariables = {
    # If your cursor becomes invisible in Hyprland
    WLR_NO_HARDWARE_CURSORS = "1";
    # Hint electron apps to use wayland
    NIXOS_OZONE_WL = "1";
  };

  # Graphics Hardware
  hardware = {
    graphics.enable = true;

    nvidia = {
      open = true;

      # Most wayland compositors need this
      modesetting.enable = true;
    };
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
