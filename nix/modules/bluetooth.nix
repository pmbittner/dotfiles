{ ... }:
{
  # Bluetooth stack (BlueZ). Without this, no adapter shows up at all.
  hardware.bluetooth = {
    enable = true;
    powerOnBoot = true; # adapter is on after boot; toggle it in blueman
  };

  # Blueman: installs blueman-manager (the GTK window for pairing, trusting,
  # connecting and removing devices) and the D-Bus service it needs.
  # Opened by clicking the bluetooth module in waybar. We do not use the tray
  # applet (blueman-applet), as waybar shows the status itself.
  services.blueman.enable = true;
}
