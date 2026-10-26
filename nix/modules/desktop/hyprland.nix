{ pkgs, unstable, username, ... }:
{
  # Use Hyprland
  programs.hyprland = {
    enable = true;
    package = unstable.hyprland;
    # Keep the portal in sync with the Hyprland version.
    portalPackage = unstable.xdg-desktop-portal-hyprland;
    xwayland.enable = true;
  };
  # No login screen: greetd starts Hyprland directly as the user on boot.
  services.greetd = {
    enable = true;
    settings = {
      default_session = {
        command = "start-hyprland";
        user = username;
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

  environment.systemPackages = with pkgs; [
    #### hyprland
    ## Lua LSP
    lua-language-server
    ## Notifications
    libnotify
    ## Screenshots (bin/screenshot.sh): wayfreeze freezes the screen, slurp
    ## selects a region, grim captures it, swappy shows and edits it.
    grim
    slurp
    swappy
    wayfreeze
    ## Wallpapers: choose exactly one of
    # hyprpaper
    # swaybg
    # wpaperd
    # mpvpaper
    unstable.awww
    ## Thumbnails for the wallpaper menu (bin/wallpaper.sh menu): vipsthumbnail
    vips
  ];
}
