{ ... }:
{
  # Notification daemon. It is started by D-Bus when the first notification
  # arrives (test with `notify-send hello`). Colors and fonts come from stylix
  # (see ../modules/style.nix).
  services.dunst.enable = true;
}
