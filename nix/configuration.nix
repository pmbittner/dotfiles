{ config, pkgs, ... }:
let
  # lon pins sources that are not part of nixpkgs (./lon.nix, ./lon.lock):
  # home-manager, stylix and lanzaboote (Secure Boot, disabled for now, see
  # boot section below). Update the pins with `pb-nixos-update-pins`.
  # Their NixOS modules are imported here, in one place. (They cannot be
  # passed to other modules via _module.args, since imports must not depend
  # on module arguments.)
  sources = import ./lon.nix;
  # lanzaboote = import sources.lanzaboote {
  #   inherit pkgs;
  # };

  # Name of this machine. Modules read it as `config.networking.hostName`.
  hostname = "perry";

  # The one user of this machine. Passed to all modules as `username`.
  username = "paul";

  unstable = import <nixpkgs-unstable> {
    config = config.nixpkgs.config;
  };
in
{
  _module.args = {
    inherit unstable username;
  };
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix

      # Pinned with lon (see above)
      "${sources.home-manager}/nixos" # used in modules/home.nix
      (import sources.stylix).nixosModules.stylix # used in modules/style.nix
      # lanzaboote.nixosModules.lanzaboote

      ./modules/hardware/nvidia.nix
      ./modules/hardware/monitors.nix
      ./modules/files.nix
      ./modules/network.nix
      ./modules/bluetooth.nix
      ./modules/sound.nix
      ./modules/home.nix
      ./modules/style.nix

      # Desktop. Other desktops would go into ./modules/desktop/ as well;
      # import exactly one of them (switching needs a reboot).
      ./modules/desktop/hyprland.nix
    ];

  # Bootloader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # Secure Boot via lanzaboote (pinned with lon in ./lon.nix and ./lon.lock).
  # Disabled for now: in the Windows dual boot, both systems reported Secure
  # Boot as active, but some games on Windows still crashed.
  # To re-enable: uncomment the lanzaboote lines at the top and in imports,
  # the block below and sbctl in systemPackages, add `lib` to the arguments
  # of this file, and replace the systemd-boot line above with
  #   boot.loader.systemd-boot.enable = lib.mkForce false;
  # since lanzaboote replaces the systemd-boot module.
  # boot.lanzaboote = {
  #   enable = true;
  #   pkiBundle = "/var/lib/sbctl"; # path to where we generated our keys
  # };

  networking.hostName = hostname;

  # Monitors of this machine (see modules/hardware/monitors.nix).
  dotfiles.monitors = {
    main = "DP-4";
    left = "DP-2";
  };

  # Set your time zone.
  time.timeZone = "Europe/Berlin";

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";

  i18n.extraLocaleSettings = {
    LC_ADDRESS = "de_DE.UTF-8";
    LC_IDENTIFICATION = "de_DE.UTF-8";
    LC_MEASUREMENT = "de_DE.UTF-8";
    LC_MONETARY = "de_DE.UTF-8";
    LC_NAME = "de_DE.UTF-8";
    LC_NUMERIC = "de_DE.UTF-8";
    LC_PAPER = "de_DE.UTF-8";
    LC_TELEPHONE = "de_DE.UTF-8";
    LC_TIME = "de_DE.UTF-8";
  };

  # Keyboard layout. This is shared by all desktops and by the console
  # (via console.useXkbConfig), so it lives here and not in a desktop module.
  # Setting it does not enable X11.
  services.xserver.xkb = {
    layout = "de";
    variant = "";
    options = "caps:escape";
  };

  # Configure console keymap
  console.useXkbConfig = true;

  programs.firefox.enable = true;

  # zsh as login shell. oh-my-zsh in ~/.zshrc runs compinit itself,
  # so skip the global one to avoid running it twice.
  programs.zsh = {
    enable = true;
    enableGlobalCompInit = false;
  };

  # direnv with nix-direnv (enabled by default) and shell hooks
  programs.direnv.enable = true;

  # Allow unfree packages
  nixpkgs.config.allowUnfree = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.${username} = {
    isNormalUser = true;
    description = "Paul Bittner";
    extraGroups = [ "networkmanager" "wheel" ];
    shell = pkgs.zsh;
    packages = with pkgs; [
      lm_sensors # for checking CPU temp
      kitty
      ranger

      vlc

      # Emacs
      emacs
      ripgrep
      fd
      clang

      shellcheck
      nixfmt

      # fun
      (pkgs.callPackage ./packages/pokemon-colorscripts.nix {})
    ];
  };

  environment.systemPackages = with pkgs; [
    # BOOT stuff (Secure Boot, currently disabled)
    # sbctl

    # Absolute Basics
    vim
    wget
    git
    gnupg
    gnumake
    lon # pins sources outside of nixpkgs, see pb-nixos-update-pins
    usbutils
    jmtpfs

    # Basics
    # fzf
    skim

    nixd # Nix LSP
    # nil # another Nix LSP
  ];

  # System fonts. Icons in waybar and elsewhere come from the Nerd Font.
  fonts.packages = with pkgs; [
    nerd-fonts.jetbrains-mono
    dejavu_fonts
  ];

  # Settings store of GTK apps. Needed by the stylix GTK target (see
  # docs/STYLE.md), do not remove.
  programs.dconf.enable = true;

  # Release of the first install of this system. Never change it, not even
  # on NixOS upgrades (see `man configuration.nix`).
  system.stateVersion = "25.11";

}
