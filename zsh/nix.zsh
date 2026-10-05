# Nix and NixOS
# Rebuilding and maintaining NixOS, and running programs with nix-shell.
# Sourced by ~/.zshrc.

### NixOS
pb-nixos-rebuild-switch () {
  sudo nixos-rebuild -I nixos-config=$HOME/nix/configuration.nix switch
}
pb-nixos-update () {
  sudo nix-channel --update
  pb-nixos-rebuild-switch
}
pb-nixos-update-pins () {
  # Updates the sources pinned with lon (stylix, home-manager) to the newest
  # commit of their branch. The pins are stored in nix/lon.lock. Sources marked
  # as frozen there (lanzaboote) are skipped. Afterwards, review the change
  # with `config diff nix/lon.lock`, then run pb-nixos-rebuild-switch and
  # commit the lock file.
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
pb-nixos-version () {
  for chan in nixos nixpkgs; do
    printf "%s: %s\n" $chan $(nix-instantiate --eval --expr "(import <$chan> {}).lib.version" 2>/dev/null);
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
