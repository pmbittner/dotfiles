{ pkgs, ... }:
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

  # Viewers and the file types they open by default.
  environment.systemPackages = with pkgs; [
    qimgv # image viewer
    evince # pdf reader
  ];
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
}
