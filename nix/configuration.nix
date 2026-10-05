{ config, pkgs, lib, ... }:
let
  # lon pins sources that are not part of nixpkgs (./lon.nix, ./lon.lock).
  # Currently pinned: stylix and home-manager (used in modules/style.nix) and
  # lanzaboote (Secure Boot, disabled for now, see boot section below).
  # Update the pins with `pb-nixos-update-pins`.
  # sources = import ./lon.nix;
  # lanzaboote = import sources.lanzaboote {
  #   inherit pkgs;
  # };
  unstable = import <nixpkgs-unstable> {
    config = config.nixpkgs.config;
  };
in
{
  _module.args = {
    inherit unstable;
  };
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
      # lanzaboote.nixosModules.lanzaboote
      ./modules/hardware/nvidia.nix
      ./modules/network.nix
      ./modules/bluetooth.nix
      ./modules/sound.nix
      ./modules/home.nix
      ./modules/style.nix

      # Choose exactly one of the following desktops.
      # You have to reboot once you switch.
      ./modules/desktop/hyprland.nix
      # ./modules/desktop/xmonad.nix
    ];

  # Bootloader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # Secure Boot via lanzaboote (pinned with lon in ./lon.nix and ./lon.lock).
  # Disabled for now: in the Windows dual boot, both systems reported Secure
  # Boot as active, but some games on Windows still crashed.
  # To re-enable: uncomment the lanzaboote lines at the top and in imports,
  # the block below and sbctl in systemPackages, and replace the
  # systemd-boot line above with
  #   boot.loader.systemd-boot.enable = lib.mkForce false;
  # since lanzaboote replaces the systemd-boot module.
  # boot.lanzaboote = {
  #   enable = true;
  #   pkiBundle = "/var/lib/sbctl"; # path to where we generated our keys
  # };

  networking.hostName = "perry"; # Define your hostname.
  networking.wireless.enable = false;  # Enables wireless support via wpa_supplicant.

  # Enable networking
  networking.networkmanager.enable = true;

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
  users.users.paul = {
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

    # some basic applications
    qimgv # image viewer
    evince # pdf reader
  ];

  fonts.packages = with pkgs; [
    nerd-fonts.jetbrains-mono
    dejavu_fonts
    font-awesome
    material-design-icons
    weather-icons
  ];

  # USB access
  services.udisks2.enable = true;
  # services.devmon.enable = true;
  # security.polkit.enable = true;
  services.gvfs.enable = true; # Mount, trash, and other functionalities

  # Default programs
  programs.thunar.enable = true;
  programs.dconf.enable = true;
  programs.xfconf.enable = true;
  services.tumbler.enable = true; # Thumbnail support for images

  xdg.mime = {
    enable = true;
    defaultApplications = {
      "image/jpeg" = "qimgv.desktop";
      "image/jpg"  = "qimgv.desktop";
      "image/png"  = "qimgv.desktop";
      "image/gif"  = "qimgv.desktop";
      "image/webp" = "qimgv.desktop";
    };
  };

  # Release of the first install of this system. Never change it, not even
  # on NixOS upgrades (see `man configuration.nix`).
  system.stateVersion = "25.11";

}
