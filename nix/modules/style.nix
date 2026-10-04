{ pkgs, ... }:
let
  # stylix and home-manager are pinned with lon (see ../lon.nix, ../lon.lock).
  sources = import ../lon.nix;
in
{
  # All Nix-based styling lives in this file.
  #
  # Stylix generates app themes from one color scheme. Most of its targets run
  # through home-manager, so home-manager is used here only for what is
  # themed by stylix. Everything else (zsh, kitty, doom, ranger, nvim, ...) stays
  # a plain dotfile that also works on machines without Nix.
  #
  # Managed by home-manager (only on perry):
  #   - GTK settings and CSS (thunar and all other GTK apps)
  # Not managed by home-manager: everything else, see README.
  imports = [
    (import sources.stylix).nixosModules.stylix
    "${sources.home-manager}/nixos"
  ];

  stylix = {
    enable = true;
    polarity = "light";
    # To switch themes, change the scheme here (and polarity if needed).
    # All schemes: https://github.com/tinted-theming/schemes (base16/)
    base16Scheme = "${pkgs.base16-schemes}/share/themes/one-light.yaml";

    # Only explicitly enabled targets are themed. Targets are enabled on the
    # home-manager side below, as they are separate from the NixOS ones.
    autoEnable = false;

    cursor = {
      package = pkgs.bibata-cursors;
      name = "Bibata-Modern-Ice";
      size = 24;
    };

    icons = {
      enable = true;
      package = pkgs.papirus-icon-theme;
      light = "Papirus";
      dark = "Papirus-Dark";
    };
  };

  home-manager = {
    useGlobalPkgs = true; # use the system nixpkgs (and its overlays)
    useUserPackages = true;
    # If a file that home-manager wants to manage already exists, it is moved
    # to <name>.hm-backup instead of aborting the rebuild.
    backupFileExtension = "hm-backup";
    users.paul = {
      # Never change this to a newer release; it is the release of the first
      # home-manager setup, like system.stateVersion.
      home.stateVersion = "25.11";

      # Targets themed by stylix. Add new ones here.
      stylix.targets.gtk.enable = true;
    };
  };
}
