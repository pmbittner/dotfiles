#!/usr/bin/env bash
#
# Switches the desktop theme at runtime (see docs/STYLE.md).
#
# Every theme from nix/themes.nix is built as a home-manager specialisation;
# switching runs its activation script. Choosing a theme and applying it are
# separate steps: the choice is recorded immediately (so waybar and the
# notification show it at once, even when cycling fast), and only one
# activation runs at a time, always for the latest choice.
#
# Usage: theme.sh menu | next | prev | set <name> | restore | current | waybar

set -euo pipefail

# Written by nix/modules/style.nix.
THEMES=/etc/dotfiles/themes                    # "name|label" per line
DEFAULT=$(cat /etc/dotfiles/theme-default)
BASE=$(cat /etc/dotfiles/home-generation)      # generation of the default theme

STATE_DIR="${XDG_STATE_HOME:-$HOME/.local/state}/dotfiles"
CHOSEN_FILE="$STATE_DIR/theme"                 # the theme the user chose
APPLIED_FILE="$STATE_DIR/theme-applied"        # the theme that is active
LOG_FILE="$STATE_DIR/activate.log"
mkdir -p "$STATE_DIR"

WAYBAR_SIGNAL=8 # must match "signal" of custom/theme in nix/modules/home/waybar.nix

names() { cut -d'|' -f1 "$THEMES"; }
label() { awk -F'|' -v name="$1" '$1 == name { print $2 }' "$THEMES"; }
exists() { names | grep -qx -- "$1"; }

# Reads a theme from a state file; the default if missing or unknown.
read_theme() {
    local name
    name=$(cat "$1" 2>/dev/null || true)
    if [ -n "$name" ] && exists "$name"; then echo "$name"; else echo "$DEFAULT"; fi
}
chosen() { read_theme "$CHOSEN_FILE"; }
applied() { read_theme "$APPLIED_FILE"; }

refresh_waybar() { pkill -RTMIN+"$WAYBAR_SIGNAL" -x waybar || true; }

notify() {
    # The stack tag makes a new theme notification replace the previous one.
    notify-send -h string:x-dunst-stack-tag:theme "$@" || true
}

# Runs the activation script of a theme's home-manager generation.
# Home-manager itself reloads dunst, and waybar reloads its style when the
# activation touches its reload trigger (see waybar.nix). Hyprland's config
# is reloaded here.
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
    echo "$name" >"$APPLIED_FILE"
    hyprctl reload >/dev/null 2>&1 || true
}

# Activates the chosen theme until chosen and applied theme agree. Only one
# process does this at a time (lock file); others wait until it is done, so
# a choice made during an activation is applied right after it.
apply_chosen() {
    local target
    while [ "$(chosen)" != "$(applied)" ]; do
        (
            flock -n 9 || exit 0
            while [ "$(chosen)" != "$(applied)" ]; do
                target=$(chosen)
                activate "$target" || exit 1
            done
        ) 9>"$STATE_DIR/apply.lock" || break
        sleep 0.2
    done
    refresh_waybar
}

# Records a choice, shows it at once, then applies it.
choose() {
    local name=$1
    exists "$name" || { echo "Unknown theme: $name" >&2; exit 1; }
    echo "$name" >"$CHOSEN_FILE"
    refresh_waybar
    notify "Theme" "$(label "$name")"
    apply_chosen
}

# Moves by $1 (1 or -1) through the list, wrapping around. The lock makes
# fast consecutive scrolls step one theme each.
step() {
    local list i count current
    mapfile -t list < <(names)
    count=${#list[@]}
    exec 7>"$STATE_DIR/choose.lock"
    flock 7
    current=$(chosen)
    for i in "${!list[@]}"; do
        [ "${list[$i]}" = "$current" ] && break
    done
    echo "${list[$(( (i + $1 + count) % count ))]}" >"$CHOSEN_FILE"
    flock -u 7
    choose "$(chosen)"
}

menu() {
    local choice name
    choice=$(cut -d'|' -f2 "$THEMES" | rofi -dmenu -i -p "Theme") || exit 0
    name=$(awk -F'|' -v label="$choice" '$2 == label { print $1 }' "$THEMES")
    [ -n "$name" ] && choose "$name"
}

waybar_status() {
    local name tooltip
    name=$(chosen)
    tooltip="Theme: $(label "$name")"
    [ "$name" = "$(applied)" ] || tooltip="$tooltip (switching ...)"
    printf '{"text": "%s", "tooltip": "%s"}\n' "󰏘" "$tooltip"
}

case "${1:-}" in
    menu)    menu ;;
    next)    step 1 ;;
    prev)    step -1 ;;
    set)     choose "${2:?Usage: theme.sh set <name>}" ;;
    # After login or a rebuild, home-manager has activated the default theme.
    restore) echo "$DEFAULT" >"$APPLIED_FILE"; apply_chosen ;;
    current) label "$(chosen)" ;;
    waybar)  waybar_status ;;
    *)       echo "Usage: theme.sh menu | next | prev | set <name> | restore | current | waybar" >&2; exit 1 ;;
esac
