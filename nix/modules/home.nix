{ ... }:
let
  # home-manager is pinned with lon (see ../lon.nix, ../lon.lock).
  sources = import ../lon.nix;
in
{
  # Home-manager manages the config of programs that are only used on perry
  # (and are therefore not part of the portable dotfiles). Each program has its
  # own file in ../home/. Colors and fonts of these programs come from stylix,
  # see style.nix.
  #
  # Managed by home-manager (only on perry): see the imports below.
  # Not managed by home-manager: everything else, e.g. zsh, kitty, doom,
  # ranger, nvim and hyprland.lua. They stay plain dotfiles that also work on
  # machines without Nix.
  imports = [ "${sources.home-manager}/nixos" ];

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

      imports = [
        ../home/dunst.nix
        ../home/rofi.nix
        ../home/waybar.nix
      ];
    };
  };
}
