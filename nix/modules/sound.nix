{ pkgs, ... }:
{
  # Sound server: PipeWire, with ALSA, PulseAudio and JACK compatibility.
  # rtkit lets PipeWire get realtime priority for smooth audio.
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
    jack.enable = true;
  };

  # pavucontrol: graphical mixer for volume, output/input device, per-app
  # streams and profiles. Opened by clicking the sound module in waybar.
  # It talks to the pipewire-pulse layer (pulse.enable above).
  environment.systemPackages = with pkgs; [
    pavucontrol
  ];
}
