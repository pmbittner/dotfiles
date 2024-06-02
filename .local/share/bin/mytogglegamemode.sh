#!/usr/bin/env sh
scrPath="$HOME/.local/share/bin" # set scripts path
HYPRGAMEMODE=$(hyprctl getoption animations:enabled | sed -n '1p' | awk '{print $2}')
hyprctl notify 1 2000 0 "Toggle game mode to: $HYPRGAMEMODE"
sh "${scrPath}/mygamemode.sh" "${HYPRGAMEMODE}"
