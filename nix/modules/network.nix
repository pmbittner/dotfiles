{ pkgs, ... }:
{
  # Networking (Wi-Fi and LAN) is managed by NetworkManager with its default
  # wpa_supplicant backend. Since NixOS 26.05, NetworkManager uses the
  # wpa_supplicant of networking.wireless (it enables it itself), so do not
  # set networking.wireless.enable = false here.
  networking.networkmanager.enable = true;

  # LAN is preferred over Wi-Fi automatically: NetworkManager gives wired
  # connections a lower route metric than wireless ones. No config needed.
  #
  # Usage on the desktop:
  #   - networkmanager_dmenu: rofi menu to list/search networks, connect,
  #     and toggle Wi-Fi. Configured in ~/.config/networkmanager-dmenu/config.ini.
  #   - nm-connection-editor: GTK window to add, edit and delete saved
  #     networks. Started from the rofi menu ("Edit Connections") or directly.
  environment.systemPackages = with pkgs; [
    networkmanager_dmenu
    networkmanagerapplet # provides nm-connection-editor
  ];
}
