{ config, lib, pkgs, ... }:
let
  # Pokémon walking around in a waybar module, from its own repository
  # (https://github.com/pmbittner/waybar-pokemon), pinned with lon.
  sources = import ../../lon.nix;
  package = pkgs.callPackage sources.waybar-pokemon { };
  exe = lib.getExe package;

  # Settings of the daemon; the waybar module below is derived from them.
  settings = {
    width = 240; # pixels
    height = 30; # fits the bar height in waybar.nix
    fps = 10;
    frame = "/dev/shm/waybar-pokemon-${config.home.username}/frame.png";
    cache_all_sprites = true; # spawn without internet
    random_generations = [ 1 2 3 4 ]; # like pokemon-colorscripts in zsh
    wild_berries_per_hour = 1; # 0 = off
    names_language = "de"; # Pokémon and berry names as in the German games
    ui_language = "de"; # notifications and menus
  };
in
{
  options.dotfiles.waybarPokemon.module = lib.mkOption {
    type = lib.types.attrs;
    readOnly = true;
    description = "Config of the waybar image module that shows the Pokémon.";
  };

  config = {
    home.packages = [ package ];

    # Read by `waybar-pokemon daemon`, which Hyprland starts (hyprland.lua).
    xdg.configFile."waybar-pokemon/config.json".text = builtins.toJSON settings;

    # Used in waybar.nix. Click: choose a Pokémon to spawn, middle click: a
    # random one, right click: choose one to despawn, scroll up: drop a berry.
    dotfiles.waybarPokemon.module = {
      path = settings.frame;
      size = settings.width; # waybar fits the image into size x size
      interval = 1.0 / settings.fps;
      tooltip = false;
      on-click = "${exe} spawn-menu --notify";
      on-click-middle = "${exe} spawn --notify";
      on-click-right = "${exe} despawn-menu --notify";
      on-scroll-up = "${exe} berry";
    };
  };
}
