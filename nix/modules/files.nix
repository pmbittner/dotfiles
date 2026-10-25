{ lib, pkgs, ... }:
let
  # Default programs: which program opens which file types. This is the
  # single place to change them. Thunar, Firefox, kitty, xdg-open (and with
  # it ranger and pb-open) follow it. See docs/DESKTOP.md, "Default programs".
  #
  # Keys are a program's desktop entry (the .desktop files in
  # /run/current-system/sw/share/applications and
  # /etc/profiles/per-user/<user>/share/applications); values are the file
  # (MIME) types it opens. `pb-mime <file>` prints the type of a file and
  # its current default program. Each type may appear only once.
  openWith = {
    "org.gnome.Evince.desktop" = [ "application/pdf" ];
    "emacsclient.desktop" = [ "text/plain" "text/markdown" ];
    "firefox.desktop" = [
      "text/html"
      "x-scheme-handler/http"
      "x-scheme-handler/https"
    ];
    "vlc.desktop" = [
      "video/mp4"
      "video/x-matroska"
      "video/vnd.avi"
    ];
    "qimgv.desktop" = [
      "image/jpeg"
      "image/jpg"
      "image/png"
      "image/gif"
      "image/webp"
    ];
  };

  # The same as type -> program, the format of xdg.mime.defaultApplications.
  defaultApplications = lib.concatMapAttrs (
    app: types: lib.genAttrs types (_: app)
  ) openWith;
  allTypes = lib.concatLists (lib.attrValues openWith);
  duplicateTypes = lib.filter (t: lib.count (x: x == t) allTypes > 1) (lib.unique allTypes);
in
{
  # Files: mounting removable drives, the file manager and the default
  # programs to open files with. Shared by all desktops.

  # Mounting USB drives and phones, trash and other functionality for
  # file managers.
  services.udisks2.enable = true;
  services.gvfs.enable = true;

  # File manager. xfconf stores Thunar's settings, tumbler creates the
  # thumbnails of images.
  programs.thunar.enable = true;
  programs.xfconf.enable = true;
  services.tumbler.enable = true;

  # Viewers (their file types are in openWith above).
  environment.systemPackages = with pkgs; [
    qimgv # image viewer
    evince # pdf reader
  ];
  xdg.mime = {
    enable = true;
    inherit defaultApplications;
  };
  assertions = [
    {
      assertion = duplicateTypes == [ ];
      message = "files.nix: these file types are assigned to more than one program in openWith: ${toString duplicateTypes}";
    }
  ];
}
