{ pkgs, ... }:
{
  # The sound server itself (PipeWire with PulseAudio compatibility) is set up
  # in modules/desktop/hyprland.nix. This module adds the graphical mixer.
  #
  # pavucontrol: volume, output/input device, per-app streams and profiles.
  # Opened by clicking the sound module in waybar. It talks to the
  # pipewire-pulse layer (services.pipewire.pulse.enable).
  environment.systemPackages = with pkgs; [
    pavucontrol
  ];
}
