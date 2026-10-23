#!/usr/bin/env bash

# Helper script to take a screenshot of a region and open it in swappy.
# The screen is frozen (wayfreeze) while the region is selected (slurp), so
# videos and animations stand still. Escape cancels the selection.

# Second stage, started by wayfreeze once the screen is frozen: select a
# region, save it to the file given as second argument, and unfreeze.
if [ "${1:-}" = "--capture" ]; then
    geometry=$(slurp) && grim -g "$geometry" "$2"
    pkill -x wayfreeze
    exit 0
fi

file=$(mktemp --suffix=.png)
trap 'rm -f "$file"' EXIT

wayfreeze --after-freeze-cmd "$(printf '%q ' "$0" --capture "$file")"

# The file is empty if the selection was cancelled.
if [ -s "$file" ]; then
    swappy -f "$file"
fi
