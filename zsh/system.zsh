# System and devices
# Kernel, network connections, USB devices and phones.
# Sourced by ~/.zshrc.

### linux
pb-get-kernel () {
  uname -mrs
}

alias connect="nmcli con up"
alias disconnect="nmcli con down"

pb-show-usb-devices () {
  nix-shell -p usbutils --run "lsusb"
}

## USB stick
ANDROID_MOUNT_DIR="$HOME/ANDROID"
pb-mount () {
  sudo mount /dev/sda1 ~/usb/
}
pb-android-mount () {
    mkdir "$ANDROID_MOUNT_DIR" && jmtpfs "$ANDROID_MOUNT_DIR" || rmdir "$ANDROID_MOUNT_DIR"
}
pb-android-unmount () {
    fusermount -u "$ANDROID_MOUNT_DIR"
    rmdir "$ANDROID_MOUNT_DIR"
}
