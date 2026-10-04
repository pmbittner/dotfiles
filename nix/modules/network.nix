{ pkgs, ... }:
{
  # Networking (Wi-Fi and LAN) is managed by NetworkManager with its default
  # wpa_supplicant backend. The core setup lives in configuration.nix:
  #   networking.networkmanager.enable = true;
  #   networking.wireless.enable = false;  # no standalone wpa_supplicant
  #
  # LAN is preferred over Wi-Fi automatically: NetworkManager gives wired
  # connections a lower route metric than wireless ones. No config needed.
  #
  # Usage on the desktop (no bar or tray needed):
  #   - networkmanager_dmenu: rofi menu to list/search networks, connect,
  #     and toggle Wi-Fi. Configured in ~/.config/networkmanager-dmenu/config.ini.
  #   - nm-connection-editor: GTK window to add, edit and delete saved
  #     networks. Started from the rofi menu ("Edit Connections") or directly.
  environment.systemPackages = with pkgs; [
    networkmanager_dmenu
    networkmanagerapplet # provides nm-connection-editor
  ];
}
