{ ... }:
let
  # home-manager is pinned with lon (see ../lon.nix, ../lon.lock).
  sources = import ../lon.nix;
in
{
  # Home-manager manages the config of programs that are only used on perry
  # (and are therefore not part of the portable dotfiles). Each program has its
  # own file in ./home/, listed in the imports below. Colors and fonts of these
  # programs come from stylix, see style.nix. What is managed and why is
  # documented in docs/STYLE.md.
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
        ./home/dunst.nix
        ./home/kitty.nix
        ./home/rofi.nix
        ./home/waybar.nix
        ./home/wlogout.nix
      ];
    };
  };
}
