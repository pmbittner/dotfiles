# Nix and NixOS
# Rebuilding and maintaining NixOS, and running programs with nix-shell.
# Sourced by ~/.zshrc.

### NixOS
# Store path of a source pinned with lon in ~/nix/lon.lock, e.g.
# `pb-nix-pinned-source nixpkgs`. Downloads it first if needed.
pb-nix-pinned-source () {
  nix-instantiate --eval --expr "toString (import $HOME/nix/lon.nix).$1" | tr -d '"'
}
# Builds the system from the pinned nixpkgs (not from a channel).
pb-nixos-rebuild-switch () {
  local nixpkgs
  nixpkgs=$(pb-nix-pinned-source nixpkgs) || return 1
  sudo nixos-rebuild -I nixpkgs="$nixpkgs" -I nixos-config=$HOME/nix/configuration.nix switch
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
  # Deletes any generation older than five days
  sudo nix-env --delete-generations --profile /nix/var/nix/profiles/system 5d
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
