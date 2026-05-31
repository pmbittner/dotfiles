{ pkgs, ... }:
{
  services.xserver = {
    enable = true;

    # Configure keymap in X11
    xkb = {
      layout = "de";
      variant = "";
      options = "caps:escape";
    };

    # Enable the X11 windowing system
    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
      extraPackages = haskellPackages: [
        haskellPackages.dbus
#       haskellPackages.xmonad-spotify
      ];
    };

    dpi = 96;
    videoDrivers = [ "nvidia" ];

    displayManager = {
      startx.enable = true;
      lightdm.enable = true; # login manager
      sessionCommands = ''
        ${pkgs.xorg.xsetroot}/bin/xsetroot -cursor_name left_ptr
        '';
    };
  };

  hardware = {
    graphics.enable = true;

    nvidia = {
      open = true;

      # Most wayland compositors need this
      modesetting.enable = true;
    };
  };

  # # Use xmonad without anything else under the hood like GNOME or something like that.
  # services.displayManager.defaultSession = "none+xmonad";
  # We can always boot into xterm in case we get lock out of xmonad. This appears as a new user.
  services.xserver.desktopManager.xterm.enable = true;

  environment.systemPackages = with pkgs; [
    ## Wallpapers
    nitrogen
    ## XMobar
    xmobar
    dmenu
    # rofi
  ];
}
