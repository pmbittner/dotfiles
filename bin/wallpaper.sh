#!/usr/bin/env bash

# Switches the wallpaper.
#   wallpaper.sh [next]   switch to the next wallpaper in $WALLPAPER_HOME
#   wallpaper.sh menu     choose a wallpaper from a grid of previews in rofi

# Directory where I store all my wallpapers.
# Hyprland sets WALLPAPER_HOME; the fallback makes this work from a terminal, too.
WALLPAPER_HOME="${WALLPAPER_HOME:-$HOME/Media/Wallpaper}"

# Cache file to remember which wallpaper is currently shown.
# We store a single integer in that file: the position in the sorted list.
STATE_FILE="$HOME/.cache/pb_current_wallpaper_index"

# Small copies of the wallpapers for the menu. Rofi would otherwise load every
# full-size image, which is slow, and webp may not load at all.
THUMBNAIL_DIR="$HOME/.cache/pb_wallpaper_thumbnails"

# Size of the previews in the menu (16:9) and the number of columns and rows.
# The thumbnails are made exactly this large, so rofi shows them pixel for pixel;
# if it had to scale them, they would look blurry.
THUMB_WIDTH=320
THUMB_HEIGHT=180
COLUMNS=4
ROWS=2

# Get a sorted list of all wallpapers in WALLPAPER_HOME.
# The position in this list is what STATE_FILE and the menu refer to.
mapfile -t wallpapers < <(find "$WALLPAPER_HOME" -type f \( -iname "*.jpg" -o -iname "*.png" -o -iname "*.jpeg" -o -iname "*.webp" \) | sort)

# Exit if there are no wallpapers.
count=${#wallpapers[@]}
[ "$count" -gt 0 ] || exit 1

# Position of the current wallpaper, or -1 if none was chosen yet.
current_index() {
    if [ -f "$STATE_FILE" ]; then
        cat "$STATE_FILE"
    else
        echo -1
    fi
}

# Shows the wallpaper at position $1 and remembers it.
set_wallpaper() {
    awww img "${wallpapers[$1]}" --transition-fps 144 --transition-type wipe --transition-angle 120 --transition-duration 1.4
    echo "$1" > "$STATE_FILE"
}

next() {
    set_wallpaper $(( ($(current_index) + 1) % count ))
}

menu() {
    mkdir -p "$THUMBNAIL_DIR"

    # One thumbnail per wallpaper. Its name contains a hash of the path, the
    # modification time and the thumbnail size, so a changed wallpaper or a
    # changed size gets a new thumbnail.
    local file thumb i
    local -a thumbs=() missing=()
    local -A used=()
    for file in "${wallpapers[@]}"; do
        thumb="$THUMBNAIL_DIR/$(printf '%s' "$file" | md5sum | cut -d' ' -f1)-$(stat -c %Y "$file")-${THUMB_WIDTH}x${THUMB_HEIGHT}.jpg"
        thumbs+=("$thumb")
        used["$thumb"]=1
        [ -f "$thumb" ] || missing+=("$file" "$thumb")
    done

    # Create the missing thumbnails, several at once. Cropped to 16:9, so that
    # all tiles in the grid are equally large.
    if [ "${#missing[@]}" -gt 0 ]; then
        notify-send "Wallpaper" "Creating previews for $(( ${#missing[@]} / 2 )) wallpapers"
        export THUMB_SIZE="${THUMB_WIDTH}x${THUMB_HEIGHT}"
        # The single quotes are intended: $1, $2 and $THUMB_SIZE belong to the
        # inner shell.
        # shellcheck disable=SC2016
        printf '%s\0' "${missing[@]}" | xargs -0 -n 2 -P "$(nproc)" \
            sh -c 'vipsthumbnail "$1" --size "$THUMB_SIZE" --smartcrop attention -o "$2[Q=90]"' _
    fi

    # Delete the thumbnails of wallpapers that do not exist anymore.
    for thumb in "$THUMBNAIL_DIR"/*.jpg; do
        if [ -e "$thumb" ] && [ -z "${used[$thumb]:-}" ]; then
            rm -f "$thumb"
        fi
    done

    # A grid of previews with the file name below each. The wallpaper that is
    # shown now is preselected. The icon widget is not squared, so the previews
    # do not leave empty space above and below.
    # The preview is aligned to the top left of its space: rofi places an
    # image at (free space * alignment), which is a fraction of a pixel for a
    # centered image, and cairo smooths an image placed that way.
    # The window width is computed from the tile size: a tile is the preview
    # plus 10px padding on each side; between tiles are 12px, around the grid
    # 12px padding and 2px border, plus some spare pixels. A window that is
    # even slightly too narrow makes rofi shrink the previews.
    local selected choice width
    width=$(( COLUMNS * (THUMB_WIDTH + 20) + (COLUMNS - 1) * 12 + 2 * 12 + 2 * 2 + 8 ))
    selected=$(current_index)
    [ "$selected" -ge 0 ] || selected=0
    choice=$(
        for i in "${!wallpapers[@]}"; do
            file=${wallpapers[$i]##*/}
            printf '%s\0icon\x1f%s\n' "${file%.*}" "${thumbs[$i]}"
        done | rofi -dmenu -i -p Wallpaper -show-icons -format i -selected-row "$selected" -theme-str "
            window { width: ${width}px; }
            listview { columns: $COLUMNS; lines: $ROWS; flow: horizontal; fixed-height: true; spacing: 12px; }
            element { orientation: vertical; padding: 10px; spacing: 6px; }
            element-icon { size: ${THUMB_WIDTH}px; squared: false; expand: false; horizontal-align: 0; vertical-align: 0; }
            element-text { horizontal-align: 0.5; }
        "
    ) || exit 0
    [ -n "$choice" ] && set_wallpaper "$choice"
}

case "${1:-next}" in
    next) next ;;
    menu) menu ;;
    *)
        echo "Usage: ${0##*/} [next|menu]" >&2
        exit 1
        ;;
esac
