#!/usr/bin/env sh
HYPRGAMEMODE=$1
# HYPRGAMEMODE=$(hyprctl getoption animations:enabled | sed -n '1p' | awk '{print $2}')

# Waybar performance
FILE="$HOME/.config/waybar/style.css"

sed -i 's/\/\* \(.*animation:.*\) \*\//\1/g' $FILE
sed -i 's/\/\* \(.*transition:.*\) \*\//\1/g' $FILE
if [ $HYPRGAMEMODE = 1 ]; then
	sed -i 's/^\(.*animation:.*\)$/\/\* \1 \*\//g' $FILE
	sed -i 's/^\(.*transition:.*\)$/\/\* \1 \*\//g' $FILE
fi
killall waybar
waybar >/dev/null 2>&1 &

# Hyprland performance
if [ $HYPRGAMEMODE = 1 ]; then
	hyprctl --batch "\
        keyword animations:enabled 0;\
        keyword general:border_size 1;\
        keyword general:gaps_out 0;\
        keyword general:gaps_in 0;\
        keyword decoration:drop_shadow 0;\
		keyword misc:vfr:enabled 0;\
		keyword decoration:active_opacity 1;\
		keyword decoration:inactive_opacity 1;\
		keyword decoration:fullscreen_opacity 1;\
        keyword decoration:rounding 0"
	exit
else
	hyprctl reload
fi

## This disables transparency for the current window.
# hyprctl dispatch toggleopaque
## This sets properties for all currently open windows.
## The set properties will turn of any transparency stuff.
# hyprctl clients -j | jq -r ".[].address" | xargs -I {} hyprctl setprop address:{} forceopaque 1 lock
# hyprctl clients -j | jq -r ".[].address" | xargs -I {} hyprctl setprop address:{} forceopaqueoverriden 1 lock
# hyprctl clients -j | jq -r ".[].address" | xargs -I {} hyprctl setprop address:{} alphaoverride 1 lock
# hyprctl clients -j | jq -r ".[].address" | xargs -I {} hyprctl setprop address:{} alpha 1 lock
# hyprctl clients -j | jq -r ".[].address" | xargs -I {} hyprctl setprop address:{} alphainactiveoverride 1 lock
# hyprctl clients -j | jq -r ".[].address" | xargs -I {} hyprctl setprop address:{} alphainactive 1 lock

# I removed the deactivation of blur to keep blur.
# The problem was that removing blur made all windows a lot
# more transparent which was too distracting.
# keyword decoration:blur:enabled 0;\
