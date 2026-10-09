#!/usr/bin/env bash
#
# Switches the desktop theme at runtime (see docs/STYLE.md).
#
# Every theme from nix/themes.nix is built as a home-manager specialisation;
# switching runs its activation script, then reloads waybar, Hyprland and
# kitty.
# The chosen theme is remembered and restored after login and rebuilds.
#
# Usage: theme.sh menu | next | prev | set <name> | restore | current | waybar

set -euo pipefail

# Written by nix/modules/style.nix.
THEMES=/etc/dotfiles/themes                    # "name|label" per line
DEFAULT=$(cat /etc/dotfiles/theme-default)
BASE=$(cat /etc/dotfiles/home-generation)      # generation of the default theme

STATE_DIR="${XDG_STATE_HOME:-$HOME/.local/state}/dotfiles"
STATE_FILE="$STATE_DIR/theme"
LOG_FILE="$STATE_DIR/activate.log"
mkdir -p "$STATE_DIR"

# Commands that switch themes run one after another, never at the same time.
lock() { exec 9>"$STATE_DIR/lock"; flock 9; }

names() { cut -d'|' -f1 "$THEMES"; }
label() { awk -F'|' -v name="$1" '$1 == name { print $2 }' "$THEMES"; }
exists() { names | grep -qx -- "$1"; }

# The active theme, or the default if none (or an unknown one) is stored.
current() {
    local name
    name=$(cat "$STATE_FILE" 2>/dev/null || true)
    if [ -n "$name" ] && exists "$name"; then echo "$name"; else echo "$DEFAULT"; fi
}

notify() {
    # The stack tag makes a new theme notification replace the previous one.
    notify-send -h string:x-dunst-stack-tag:theme "$@" || true
}

# Activates a theme's home-manager generation and remembers it. Home-manager
# reloads dunst itself; waybar, Hyprland and kitty are reloaded here. The state is
# written before the reload, so waybar shows the new theme.
activate() {
    local name=$1 generation
    if [ "$name" = "$DEFAULT" ]; then
        generation=$BASE
    else
        generation=$BASE/specialisation/$name
    fi
    if [ ! -x "$generation/activate" ]; then
        notify -u critical "Theme" "Theme '$(label "$name")' is not built. Rebuild the system first."
        return 1
    fi
    if ! "$generation/activate" >"$LOG_FILE" 2>&1; then
        notify -u critical "Theme" "Switching to '$(label "$name")' failed, see $LOG_FILE"
        return 1
    fi
    echo "$name" >"$STATE_FILE"
    pkill -SIGUSR2 -x waybar || true           # reloads config and style
    hyprctl reload >/dev/null 2>&1 || true     # reads nix/generated.lua again
    # kitty rereads its config, including the generated colors, in all open
    # windows; the zsh prompt follows, as it uses the terminal's palette.
    # (On NixOS, kitty's process may be called .kitty-wrapped.)
    pkill -SIGUSR1 -x 'kitty|\.kitty-wrapped' || true
}

set_theme() {
    local name=$1
    exists "$name" || { echo "Unknown theme: $name" >&2; exit 1; }
    activate "$name"
    notify "Theme" "$(label "$name")"
}

# Moves by $1 (1 or -1) through the list, wrapping around.
step() {
    local list i count
    mapfile -t list < <(names)
    count=${#list[@]}
    for i in "${!list[@]}"; do
        [ "${list[$i]}" = "$(current)" ] && break
    done
    set_theme "${list[$(( (i + $1 + count) % count ))]}"
}

menu() {
    local choice name
    choice=$(cut -d'|' -f2 "$THEMES" | rofi -dmenu -i -p "Theme") || exit 0
    name=$(awk -F'|' -v label="$choice" '$2 == label { print $1 }' "$THEMES")
    [ -n "$name" ] || exit 0
    lock
    set_theme "$name"
}

case "${1:-}" in
    menu)    menu ;;                        # locks after the choice
    next)    lock; step 1 ;;
    prev)    lock; step -1 ;;
    set)     lock; set_theme "${2:?Usage: theme.sh set <name>}" ;;
    # After login or a rebuild, home-manager has activated the default theme.
    restore) lock; [ "$(current)" = "$DEFAULT" ] || activate "$(current)" ;;
    current) label "$(current)" ;;
    # Output for waybar's custom/theme module (JSON with tooltip).
    waybar)  printf '{"text": "%s", "tooltip": "Theme: %s"}\n' "󰏘" "$(label "$(current)")" ;;
    *)       echo "Usage: theme.sh menu | next | prev | set <name> | restore | current | waybar" >&2; exit 1 ;;
esac
