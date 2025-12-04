# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:
let
  nixos-unstable = import <nixos-unstable> {
    ## use the same configuration (in particular, allow unfree software)
    config=config.nixpkgs.config;
    overlays=[];
  };
in
{
  imports = [
    # Include the results of the hardware scan.
    <nixos-hardware/dell/xps/13-7390>
    ./hardware-configuration.nix
  ];

  # Bootloader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "nixos"; # Define your hostname.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

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
    LC_ADDRESS = "en_US.UTF-8";
    LC_IDENTIFICATION = "en_US.UTF-8";
    LC_MEASUREMENT = "en_US.UTF-8";
    LC_MONETARY = "en_US.UTF-8";
    LC_NAME = "en_US.UTF-8";
    LC_NUMERIC = "en_US.UTF-8";
    LC_PAPER = "en_US.UTF-8";
    LC_TELEPHONE = "en_US.UTF-8";
    LC_TIME = "en_US.UTF-8";
  };

  # Enable the X11 windowing system.
  services.xserver = {
    enable = true;

    # Configure keymap in X11
    xkb = {
      layout = "de";
      variant = "";
      options = "caps:escape";
    };

    # Enable the GNOME Desktop Environment.
    displayManager.gdm.enable = true;
    desktopManager.gnome.enable = true;

    # Enable displaylink for my Dell Docking Station
    videoDrivers = [ "displaylink" "modesetting" ];
  };

  # Configure console keymap
  console = {
    useXkbConfig = true;
  };


  # Enable CUPS to print documents.
  services.printing.enable = true;

  # Enable sound with pipewire.
  hardware.pulseaudio.enable = false;
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
    # If you want to use JACK applications, uncomment this
    #jack.enable = true;

    # use the example session manager (no others are packaged yet so this is enabled by default,
    # no need to redefine it in your config for now)
    #media-session.enable = true;
  };

  # Enable touchpad support (enabled default in most desktopManager).
  # services.xserver.libinput.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.paul = {
    isNormalUser = true;
    description = "paul";
    extraGroups = [ "networkmanager" "wheel" ];
    packages = with pkgs; [
      zsh
      kitty
      ranger
      evince
      pdftk
      imagemagick
      texliveFull
      thunderbird
      telegram-desktop
      spotify
      vlc
      pkgs.kdePackages.kolourpaint
      pandoc
      graphviz
      powertop
      libreoffice
      inkscape
      ccrypt
      sqlite # for org-roam

      # packages from unstable channel
      nixos-unstable.signal-desktop
      nixos-unstable.zoom-us

      # programming languages
      jdk23
      jdt-language-server
      # semgrep
      # nixd # nix language server
      maven
      (agda.withPackages [
        agdaPackages.standard-library
      ])
      python39
      python3Packages.bibtexparser

      # pygmentize for LaTeX minted
      python312Packages.pygments

      # doom emacs
      emacs30
      ripgrep

      # optional doom emacs dependencies
      coreutils # basic gnu utilities
      fd
      clang

      # more doom emacs extras
      shellcheck
      (aspellWithDicts (dicts: with dicts; [ en en-computers en-science de ]))
      python312Packages.grip

      # fun
      (pkgs.callPackage ./packages/pokemon-colorscripts.nix {})

      # vs code
      (vscode-with-extensions.override {
        vscodeExtensions = with vscode-extensions; [
          ms-python.python
        ];
      })
    ];
  };

  # Install firefox.
  programs.firefox.enable = true;

  # Customize nautilus
  programs.nautilus-open-any-terminal = {
    enable = true;
    terminal = "kitty";
  };

  # Allow unfree packages
  nixpkgs.config.allowUnfree = true;

  fonts.packages = with pkgs; [
    nerdfonts
    emacs-all-the-icons-fonts
  ];

  environment.sessionVariables = {
    SHELL  = "zsh";
    EDITOR = "emacsclient -c -a 'emacs'";
    _JAVA_OPTIONS="-Dawt.useSystemAAFontSettings=lc"; # for better font rendering: https://nixos.wiki/wiki/Java
    JDTLS_PATH = "${pkgs.jdt-language-server}/share/java";
  };

  environment.pathsToLink = [
    "/share/nautilus-python/extensions"
  ];

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    # absolute basics
    git
    git-lfs
    gnumake
    ghostscript
    wget
    vim
    direnv
    fzf

    # basics
    skim
    util-linux # for setsid
    gnome-tweaks # maybe it would be cool to include this conditionally only if we have gnome desktop
    # gdm-settings # this does not work because it wants to change an ini file in the nix store
  ];

  # Run the emacs daemon on startup
  services.emacs = {
    enable = true;
    package = pkgs.emacs30;
  };

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
  system.stateVersion = "24.11"; # Did you read the comment?

}
