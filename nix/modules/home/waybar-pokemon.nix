{ config, lib, pkgs, ... }:
let
  # Pokémon walking around in a waybar module (prototype, see
  # nix/packages/waybar-pokemon/README.md).
  package = pkgs.callPackage ../../packages/waybar-pokemon { };
  exe = lib.getExe package;

  # Settings of the daemon; the waybar module below is derived from them.
  settings = {
    width = 240; # pixels
    height = 30; # fits the bar height in waybar.nix
    fps = 10;
    frame = "/dev/shm/waybar-pokemon-${config.home.username}/frame.png";
    cache_all_sprites = true; # spawn without internet
    random_generations = [ 1 2 3 4 ]; # like pokemon-colorscripts in zsh
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
    # random one, right click: choose one to despawn.
    dotfiles.waybarPokemon.module = {
      path = settings.frame;
      size = settings.width; # waybar fits the image into size x size
      interval = 1.0 / settings.fps;
      tooltip = false;
      on-click = "${exe} spawn-menu --notify";
      on-click-middle = "${exe} spawn --notify";
      on-click-right = "${exe} despawn-menu --notify";
    };
  };
}
