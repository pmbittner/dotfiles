# Nix and NixOS
# Rebuilding and maintaining NixOS, and running programs with nix-shell.
# Sourced by ~/.zshrc.

### NixOS
# Store path of a source pinned with lon in ~/nix/lon.lock, e.g.
# `pb-nix-pinned-source nixpkgs`. Downloads it first if needed.
pb-nix-pinned-source () {
  nix-instantiate --eval --expr "toString (import $HOME/nix/lon.nix).$1" | tr -d '"'
}
# Builds the system from the pinned nixpkgs through ~/nix/system.nix.
# (A plain `sudo nixos-rebuild switch` does the same through
# /etc/nixos/system.nix, once that file exists after the first rebuild.)
# Afterwards, the last chosen desktop theme is applied again, as the rebuild
# activates the default theme (see bin/theme.sh).
pb-nixos-rebuild-switch () {
  sudo nixos-rebuild switch --file "$HOME/nix/system.nix" || return
  if [ -x "$HOME/bin/theme.sh" ] && [ -n "$WAYLAND_DISPLAY" ]; then
    "$HOME/bin/theme.sh" restore
  fi
}
pb-nixos-update () {
  pb-nixos-update-pins && pb-nixos-rebuild-switch
}
pb-nixos-update-pins () {
  # Updates all sources pinned with lon (nixpkgs, nixpkgs-unstable, stylix,
  # home-manager) to the newest commit of their branch. The pins are stored in
  # nix/lon.lock. Sources marked as frozen there (lanzaboote) are skipped.
  # Afterwards, review the change with `config diff nix/lon.lock`, then run
  # pb-nixos-rebuild-switch and commit the lock file.
  (cd $HOME/nix && lon update)
}
pb-nixos-garbage-collection () {
  nix-store --gc
}
pb-nixos-show-generations () {
  sudo nix-env --list-generations --profile /nix/var/nix/profiles/system
}
pb-nixos-delete-outdated-generations () {
  # Keeps the newest N generations (default 5, the current one included) and
  # cleans up after the deleted ones:
  #  1. delete the older generations from the system profile
  #  2. refresh the boot menu, which nix-env does not touch, and delete the
  #     kernels of the deleted generations from /boot
  #  3. delete the store paths that nothing uses anymore
  local keep=${1:-5}
  sudo nix-env --delete-generations --profile /nix/var/nix/profiles/system "+$keep" || return
  sudo /nix/var/nix/profiles/system/bin/switch-to-configuration boot || return
  sudo nix-collect-garbage
}
# Versions of the pinned nixpkgs sources.
pb-nixos-version () {
  local src
  for src in nixpkgs nixpkgs-unstable; do
    printf "%s: %s\n" $src "$(nix-instantiate --eval --expr "(import $(pb-nix-pinned-source $src)/lib).version" | tr -d '"')"
  done
}
pb-nix-shell-run () {
  nix-shell -p "$@" --run "$@"
}
pb-nix-steam-run () {
  env NIXPKGS_ALLOW_UNFREE=1 nix-shell -p steam-run --run "steam-run $@"
}
alias nrs="pb-nixos-rebuild-switch"
alias ngc="pb-nixos-garbage-collection"
alias nsr="pb-nix-shell-run"
