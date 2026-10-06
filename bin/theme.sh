#!/usr/bin/env bash
#
# Switches the desktop theme at runtime (see docs/STYLE.md).
#
# Every theme from nix/themes.nix is built as a home-manager specialisation;
# switching runs its activation script and reloads the running programs.
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

names() { cut -d'|' -f1 "$THEMES"; }
label() { awk -F'|' -v name="$1" '$1 == name { print $2 }' "$THEMES"; }
exists() { names | grep -qx -- "$1"; }

# The remembered theme, or the default if none (or an unknown one) is stored.
current() {
    local name
    name=$(cat "$STATE_FILE" 2>/dev/null || true)
    if [ -n "$name" ] && exists "$name"; then echo "$name"; else echo "$DEFAULT"; fi
}

# Restarts or reloads the programs that do not pick up the new theme by
# themselves. GTK apps, rofi and wlogout use it the next time they open.
reload_programs() {
    pkill -SIGUSR2 -x waybar || true           # reloads config and style
    systemctl --user restart dunst.service || true
    hyprctl reload >/dev/null 2>&1 || true     # reads nix/generated.lua again
}

# Activates the theme's home-manager generation.
activate() {
    local name=$1 generation log="$STATE_DIR/activate.log"
    if [ "$name" = "$DEFAULT" ]; then
        generation=$BASE
    else
        generation=$BASE/specialisation/$name
    fi
    if [ ! -x "$generation/activate" ]; then
        notify-send -u critical "Theme" "Theme '$name' is not built. Rebuild the system first."
        return 1
    fi
    mkdir -p "$STATE_DIR"
    if ! "$generation/activate" >"$log" 2>&1; then
        notify-send -u critical "Theme" "Switching to '$(label "$name")' failed, see $log"
        return 1
    fi
    reload_programs
}

set_theme() {
    local name=$1
    exists "$name" || { echo "Unknown theme: $name" >&2; exit 1; }
    activate "$name"
    mkdir -p "$STATE_DIR"
    echo "$name" >"$STATE_FILE"
    notify-send "Theme" "$(label "$name")"
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
    [ -n "$name" ] && set_theme "$name"
}

case "${1:-}" in
    menu)    menu ;;
    next)    step 1 ;;
    prev)    step -1 ;;
    set)     set_theme "${2:?Usage: theme.sh set <name>}" ;;
    # After login or a rebuild, the default theme is active again.
    restore) [ "$(current)" = "$DEFAULT" ] || activate "$(current)" ;;
    current) label "$(current)" ;;
    # Output for waybar's custom/theme module (JSON with tooltip).
    waybar)  printf '{"text": "%s", "tooltip": "Theme: %s"}\n' "󰏘" "$(label "$(current)")" ;;
    *)       echo "Usage: theme.sh menu | next | prev | set <name> | restore | current | waybar" >&2; exit 1 ;;
esac
