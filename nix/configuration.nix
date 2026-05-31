# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, lib, ... }:
let
  sources = import ./lon.nix;
  lanzaboote = import sources.lanzaboote {
    inherit pkgs;
  };
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
      lanzaboote.nixosModules.lanzaboote

      # Choose exactly one of the following desktops.
      # You have to reboot once you switch.
      ./modules/desktop/hyprland.nix
      # ./modules/desktop/xmonad.nix
    ];

  # Bootloader.
  # Lanzaboote currently replaces the systemd-boot module.
  # This setting is usually set to true in configuration.nix
  # generated at installation time. So we force it to false
  # for now.
  boot.loader.systemd-boot.enable = lib.mkForce false;
  boot.loader.efi.canTouchEfiVariables = true;

  boot.lanzaboote = {
    enable = true;
    pkiBundle = "/var/lib/sbctl"; # path to where we generated our keys (?)
  };

  networking.hostName = "perry"; # Define your hostname.
  networking.wireless.enable = false;  # Enables wireless support via wpa_supplicant.

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

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

  # Configure console keymap
  console.useXkbConfig = true;

  programs.firefox.enable = true;

  # Allow unfree packages
  nixpkgs.config.allowUnfree = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.paul = {
    isNormalUser = true;
    description = "Paul Bittner";
    extraGroups = [ "networkmanager" "wheel" ];
    packages = with pkgs; [
      lm_sensors # for checking CPU temp
      zsh
      kitty
      ranger

      vlc

      # Emacs
      emacs
      ripgrep
      coreutils
      fd
      clang

      shellcheck
      nixfmt

      # fun
      (pkgs.callPackage ./packages/pokemon-colorscripts.nix {})
    ];
  };

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    # BOOT stuff
    sbctl
    lon # need that for secure boot with lanzaboote

    # Absolute Basics
    vim
    wget
    git
    gnupg
    gnumake
    usbutils
    jmtpfs

    # Basics
    # fzf
    skim
    util-linux # for setsid

    nixd # Nix LSP
    # nil # another Nix LSP

    direnv
    nix-direnv

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

  # beautify file manager
  # TODO: Replace the following with Stylix later on.
  programs.dconf.profiles.user.databases = [{
    settings = {
      "org/gnome/desktop/interface" = {
        color-scheme = "default"; # or "default" for light theme
        gtk-theme = "adw-gtk3-dark"; # Exact name from your installed theme packages
        icon-theme = "Papirus";      # Exact name of icon package
        cursor-theme = "Bibata-Modern-Ice"; # Optional cursor
      };
    };
  }];


  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  # };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  # services.openssh.enable = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "25.11"; # Did you read the comment?

}
