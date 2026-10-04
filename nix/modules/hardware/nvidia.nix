{ ... }:
{
  # Needed for both X11 and Wayland sessions.
  services.xserver.videoDrivers = [ "nvidia" ];

  hardware = {
    graphics.enable = true;

    nvidia = {
      open = true;

      # Required by Wayland compositors
      modesetting.enable = true;
    };
  };
}
