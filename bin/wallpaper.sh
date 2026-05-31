#!/usr/bin/env sh

# This file switches the wallpaper to the next wallpaper in $WALLPAPER_HOME.

# Directory where I store all my wallpapers
# Cache file to remember which wallpaper is currently shown.
# We store a single integer in that file.
STATE_FILE="$HOME/.cache/pb_current_wallpaper_index"

# Get a list of all wallpapers in WALLPAPER_HOME,
# and count how many wallpapers there are.
WALLPAPERS=$(find "$WALLPAPER_HOME" -type f \( -iname "*.jpg" -o -iname "*.png" -o -iname "*.jpeg" \) | sort)
WALLPAPER_COUNT=$(echo "$WALLPAPERS" | wc -l)

# Exit if there are no wallpapers
[ "$WALLPAPER_COUNT" -eq 0 ] && exit 1

# Read index of current wallpaper
if [ -f "$STATE_FILE" ]; then
    index=$(cat "$STATE_FILE")
else
    index=-1
fi

NEXT_INDEX=$(( (index + 1) % WALLPAPER_COUNT ))
NEXT_WALLPAPER=$(echo "$WALLPAPERS" | sed -n "$((NEXT_INDEX + 1))p")

awww img "$NEXT_WALLPAPER" --transition-fps 144 --transition-type any

# store new index
echo "$NEXT_INDEX" > "$STATE_FILE"
